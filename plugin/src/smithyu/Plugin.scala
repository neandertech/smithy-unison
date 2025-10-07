package smithyu

import software.amazon.smithy.build.SmithyBuildPlugin
import software.amazon.smithy.build.PluginContext
import software.amazon.smithy.build.SmithyBuild
import smithyu.codegen.SmithyToIR
import smithyu.codegen.renderDefinition
import smithyu.codegen.Lines.newline
import software.amazon.smithy.model.node.Node

class Plugin() extends SmithyBuildPlugin {
  override def getName(): String = "unison"

  override def execute(context: PluginContext): Unit = {
    val config               = UnisonPluginSettings.fromNode(context.getSettings())
    val model                = context.getModel()
    val definitions          = SmithyToIR(model).definitions
    val (allowed, discarded) = definitions.iterator
      .map(_.shapeId.getNamespace())
      .distinct
      .partition(config.allowsNamespace)

    val allowedMessage =
      if (allowed.isEmpty)
      then "Unison plugin misconfigured: no namespace will be rendered."
      else s"""|Will render Unison code for shapes in namespaces:
               |
               |${allowed.mkString(",\n")}
               |""".stripMargin

    val message =
      s"""|$allowedMessage
          |
          |Discarding shapes from namespaces:
          |
          |${discarded.mkString(",\n")}
          |""".stripMargin
    println(message)

    val contents = codegen
      .Lines(
        definitions
          .filter(d => config.allowsNamespace(d.shapeId.getNamespace()))
          .map(renderDefinition)
          .map(_ ++ newline)
      )
      .get
      .mkString("\n")

    context.getFileManifest().writeFile("hello.u", contents)
  }

}
