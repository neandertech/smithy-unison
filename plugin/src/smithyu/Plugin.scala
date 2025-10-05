package smithyu

import software.amazon.smithy.build.SmithyBuildPlugin
import software.amazon.smithy.build.PluginContext
import software.amazon.smithy.build.SmithyBuild
import smithyu.codegen.SmithyToIR
import smithyu.codegen.renderDefinition
import smithyu.codegen.Lines.newline

class Plugin() extends SmithyBuildPlugin {
  override def getName(): String                     = "unison"
  override def execute(context: PluginContext): Unit = {
    val model    = context.getModel()
    val contents = codegen
      .Lines(
        SmithyToIR(model).definitions.map(renderDefinition).map(_ ++ newline)
      )
      .get
      .mkString("\n")

    context.getFileManifest().writeFile("hello.u", contents)
  }

}
