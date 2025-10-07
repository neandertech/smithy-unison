package smithyu

import java.util.regex.Pattern
import software.amazon.smithy.model.node.Node
import scala.jdk.CollectionConverters.*
import software.amazon.smithy.model.node.ArrayNode
import java.util.function.Predicate

case class UnisonPluginSettings(allowsNamespace: String => Boolean)

object UnisonPluginSettings {

  def fromNode(node: Node): UnisonPluginSettings = {
    val allowsNamespace = node.expectObjectNode
      .getArrayMember("namespaces")
      .orElse(ArrayNode.builder().build())
      .getElements()
      .asScala
      .map(_.expectStringNode().getValue())
      .map(toPattern)
      .map(_.asMatchPredicate())
      .fold[Predicate[String]]((_: String) => false) { case (left, right) =>
        (str: String) => left.test(str) || right.test(str)
      }
    UnisonPluginSettings(allowsNamespace.test(_))
  }

  // Glob pattern type thing.
  private def toPattern(filter: String): Pattern = {
    val parts = filter
      .split("\\*", -1)
      .map { // Don't discard trailing empty string, if any.
        case ""  => ""
        case str => Pattern.quote(str)
      }
    Pattern.compile(parts.mkString(".*"))
  }

}
