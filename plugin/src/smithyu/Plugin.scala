package smithyu

import software.amazon.smithy.build.SmithyBuildPlugin
import software.amazon.smithy.build.PluginContext

class Plugin() extends SmithyBuildPlugin {
  override def getName(): String = "unison"
  override def execute(context: PluginContext): Unit = {
    val model = context.getModel()

    context.getFileManifest().writeFile("hello.txt", "hello")
  }

}
