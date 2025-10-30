# smithy-unison

This project is a **protocol/serialisation agnostic** code-generator that produces [Unison](https://www.unison-lang.org/) code from [smithy](https://smithy.io) models.

It works via the [smithy-cli](https://smithy.io/2.0/guides/smithy-cli/index.html), a tool to load and process smithy models.

## How to use ?

### Installation of the smithy-cli

First, you need to have the smithy-cli installed on your machine. To that end, refer yourself to [the official installation instructions](https://smithy.io/2.0/guides/smithy-cli/cli_installation.html).

### Configuration

The `smithy-cli` is configured using a JSON file in the directory where you want to run it. The format for this file is documented [here](https://smithy.io/2.0/guides/smithy-build-json.html). Typically, it should look like this :

```json
{
  "version": "1.0",
  "outputDirectory": "out",
  "sources": [
    "./model.smithy"
  ],
  "maven": {
    "dependencies": [
      "tech.neander:smithy-unison:x.y.z"
    ]
  },
  "plugins": {
    "unison": {
      "namespaces": [
        "example"
      ]
    }
  }
}
```

Here's a concise explanation of the format above :

```json
{
  // the version of the smithy-build.json configuration format
  "version": "1.0",
  // directory in which the smithy-cli will produce its artifacts and outputs
  "outputDirectory": "out",
  // files or directories containing local smithy models
  "sources": [
    "model"
  ],
  "maven": {
    "dependencies": [
      // reference to the maven coordinates of this plugin
      "tech.neander:smithy-unison:x.y.z",
      // additional artifacts, potentially containing other smithy models you want to
      // generate code for, or that your model depends on.
      "software.amazon.api.models:dynamodb:1.0.3"
    ]
  },
  "plugins": {
    "unison": {
      // list of the namespaces that this plugin should generate Unison code for.
      // Wildcards can be used.
      "namespaces": [
        "example",
        "smithy.api",
        "com.amazonaws.*"
      ]
    }
  }
}
```

### Running

Simply run the following in the directory where your `smithy-build.json` configuration file is located.

```
> smithy build
```

## What does the generated code contain ?

The generated code contains idiomatic Unison data-types representing the smithy shapes, as well as associated [`Schema`](https://share.unison-lang.org/@baccata/schemas/) values capturing details allowing to wire these types with serialisation functions. This implies that in order for the generated code
to compile, you must install this library in your project.
