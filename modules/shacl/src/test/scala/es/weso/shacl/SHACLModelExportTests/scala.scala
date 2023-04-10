package es.weso.shacl

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{DecimalLiteral, IRI, RDFNode, StringLiteral}
import es.weso.rdf.path.PredicatePath
import es.weso.shacl.converter.Shacl2RDF

import scala.collection.mutable.ListBuffer

object SHACLModelExportTests extends IOApp {

  def localName(str:String): String = {
    val dropBracketsRegex="[<>]".r
    val localNameRegex= "([^/]+)$".r
    localNameRegex.findFirstIn(dropBracketsRegex.replaceAllIn(str,"")).getOrElse("");
  }

  def propertyShapeSourceLocalName(str:String): String = {
    val propertyShapeLocalName = localName(str)
    val sourceClasslocalNameRegex= "(--[A-Za-z0-9_-]+)".r
    sourceClasslocalNameRegex.replaceFirstIn(propertyShapeLocalName,"")
  }

  def toCamelCase(str:String): String = {
    val words = """([0-9a-zA-Z_-]+)""".r
    val str2 = words.replaceAllIn(str, m => m.matched.toLowerCase.capitalize)
    val separators = """([^0-9a-zA-Z_-]+)""".r
    separators.replaceAllIn(str2, "");
  }

  def buildResourceURI(uriPreamble: Option[String] , resourceType: Option[String] ,useSlashForURI: Option[Boolean], resourceNameTransformer: (String => String), resourceName:String): IRI = {
    IRI(uriPreamble.getOrElse("http://example.com")
      + "/" + resourceType.getOrElse("schema")
      + (if (useSlashForURI.getOrElse(true)) '/' else '#')
      + resourceNameTransformer(resourceName))
  }

  def makePropertyShape(
                         associationName: String
                         , sourceClass: NodeShape
                         , targetClass: NodeShape
                         , minCardinality: Option[Int]
                         , maxCardinality:Option[Int]
                         , order :Option[DecimalLiteral]
                         , group : Option[RefNode]
                       ): (RefNode, PropertyShape) = {
    val sourceClassLocalName = localName(sourceClass.id.toString)
    val propertyLocalName = if (associationName == "") ("has" + toCamelCase(localName(targetClass.name.toString())))
    else toCamelCase(associationName).toLowerCase
    val propertyPath = PredicatePath(buildResourceURI(None,None,None,toCamelCase,propertyLocalName))
    val propertyShapeLocalName = sourceClassLocalName + "--" + propertyLocalName
    val propertyShapeNode =buildResourceURI(None,None,None,toCamelCase,propertyShapeLocalName)
    var components: ListBuffer[Component] = ListBuffer(ClassComponent(targetClass.id))
    components = if (minCardinality.nonEmpty) {components +=  MinCount(minCardinality.get)} else components
    components = if (maxCardinality.nonEmpty) {components += MaxCount(maxCardinality.get)} else components
    val aPropertyShape = PropertyShape(
      propertyShapeNode
      , propertyPath
      , components.toList
      , Seq() // targets
      , Seq() // propertyShapes
      , false // closed
      , List() // ignoredProperties
      , false // deactivated
      , MessageMap.empty // message
      , None // severity
      , MessageMap.fromString("name") // TODO name
      , MessageMap.fromString("<p>TODO: property shape description</p>") // description
      , order // order
      , group // group
      , Some(sourceClass.id.toIRI.right.get) // sourceIRI
      , List() // annotations
    )
    (RefNode(propertyShapeNode), aPropertyShape)
  }

  def makeNodeShape(name: String): (RDFNode, NodeShape) = {
    val classNode: RDFNode = buildResourceURI(None,None,None,toCamelCase, name)
    val classNodeShape = NodeShape(
      id = classNode
      , components = List()
      , targets = Seq()
      , propertyShapes = Seq()
      , closed = false
      , ignoredProperties = List()
      , deactivated = false
      , message = MessageMap.empty
      , severity = None
      , name = MessageMap.fromString(name)
      , description = MessageMap.fromString("<p>TODO: node shape description</p>") // description
      , order = None
      , group = None
      , None // sourceIRI
    )
    (classNode, classNodeShape)
  }

  def updateNodeShapeWithPropertyShapeReference(ps: PropertyShape, ns: NodeShape): NodeShape = {
    val newPS: RefNode = RefNode(ps.id)
    val nodeShape = ns.copy(propertyShapes = ns.propertyShapes :+ newPS)
    nodeShape
  }

  def makePropertyGroup(name:String): (RefNode, PropertyGroup) = {
    val propertyGroupRefNode = RefNode(buildResourceURI(None,None,None,toCamelCase,name))
    val propertyGroup = PropertyGroup(
      order = Some(DecimalLiteral(10)) //  Option[DecimalLiteral]
      , label = Set(StringLiteral(name)) // Set[RDFNode]
    )
    (propertyGroupRefNode, propertyGroup)
  }

  val propertyGroups = List ("Property Group 1","Property Group 2")
  val myPropertyGroups: Map[RefNode, PropertyGroup] =
    propertyGroups
      .map(pg => makePropertyGroup(pg))
      .toMap.map({
      case (k, v) => k -> v.asInstanceOf[PropertyGroup]
    })

  val classList = List("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")

  val myNodeShapes =
    classList
      .map(s => makeNodeShape(s))
      .map(nsPair => (RefNode(nsPair._1), nsPair._2))
      .flatMap { case (a, b) => List(a, b) }
      .grouped(2).collect { case List(k, v) => k -> v }
      .toMap.map({
      case (k, v) => k.asInstanceOf[RefNode] -> v.asInstanceOf[NodeShape]
    })

  println(myNodeShapes)

  val propertyList = List("hasRelation1", "hasRelation2", "hasRelation3", "hasRelation4", "hasRelation5")

  // make property shapes reverses the class list so that for each class the property points to a different class

  //  val myPropertyGroup = makePropertyGroup("My Property Group")._1
  val myPropertyShapes: Map[RefNode, PropertyShape] = (classList.reverse zip (propertyList zip classList))
    .map(sourcePropertyTarget => makePropertyShape(
      sourcePropertyTarget._2._1
      , myNodeShapes(RefNode(buildResourceURI(None,None,None,toCamelCase, sourcePropertyTarget._1))).asInstanceOf[NodeShape] // source class
      , myNodeShapes(RefNode(buildResourceURI(None,None,None,toCamelCase, sourcePropertyTarget._2._2))).asInstanceOf[NodeShape] // target class
      , Some(0)
      , Some(1)
      , Some(DecimalLiteral(10)) // order
      , Some(myPropertyGroups.toList.head._1)) // group
    )
    .flatMap { case (a, b) => List(a, b) }
    .grouped(2).collect { case List(k, v) => k -> v }
    .toMap
    .map({
      case (k, v) => k.asInstanceOf[RefNode] -> v.asInstanceOf[PropertyShape]
    })

  val updatedNodeShapes: Map[RefNode, NodeShape] = myPropertyShapes
    .map(ps => {
      val myPropertyShapeSourceLocalName: String = propertyShapeSourceLocalName(ps._1.id.toString)
      val mySourceNode = RefNode(buildResourceURI(None, None, None, (s => s), myPropertyShapeSourceLocalName))
      (mySourceNode, updateNodeShapeWithPropertyShapeReference(ps._2, myNodeShapes(mySourceNode)))
    }
    )
    .toMap.map({
    case (k, v) => k.asInstanceOf[RefNode] -> v.asInstanceOf[NodeShape]
  }
  )

  println(updatedNodeShapes)

  val shacl = Schema(
    pm = SHACLPrefixes.defaultPrefixMap
    , imports = List()
    , entailments = List()
    , shapesMap = updatedNodeShapes ++ myPropertyShapes
    , propertyGroups = myPropertyGroups
  )

  def run(args: List[String]) = {

    for {
      res2 <- RDFAsJenaModel.empty
      _ <- res2.use { empty =>
        for {
          _ <- IO.println(s"Number of shacl shapes = ${shacl.shapes.length}")
          rdfOut <- new Shacl2RDF().toRDF(shacl, empty)
          _ <- IO.println(s"Converting to TURTLE")
          str <- rdfOut.serialize("TURTLE")
          _ <- IO.println(str)
        } yield ()
      }
    } yield ExitCode.Success
  }
}