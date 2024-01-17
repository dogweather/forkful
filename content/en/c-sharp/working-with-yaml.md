---
title:                "Working with yaml"
html_title:           "C# recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML, also known as YAML Ain't Markup Language, is a way for programmers to define and organize data in a human-readable format. It is often used to store configuration data and can be used in various programming languages. Programmers use YAML because it provides a simple and concise way to represent complex data structures.

## How to:

To work with YAML in C#, you will need to install a YAML library such as YamlDotNet or SnakeYAML. Once imported into your project, you can use the library's methods to load and manipulate YAML data.

Let's take a look at a simple example using YamlDotNet:

```C#
//Load YAML data from a file
YamlStream stream = new YamlStream();
using (var reader = new StreamReader("config.yaml"))
{
    stream.Load(reader);
}

//Access YAML data
YamlMappingNode mapping = (YamlMappingNode)stream.Documents[0].RootNode;
string username = mapping.Children[new YamlScalarNode("username")].ToString();

//Update YAML data
mapping.Children[new YamlScalarNode("password")] = "newPassword";

//Save changes to file
using (StreamWriter writer = new StreamWriter("config.yaml"))
{
    stream.Save(writer, false);
}
```

The output of this code will be a YAML file with the updated password value. Note that the specific syntax and methods may vary depending on the YAML library used.

## Deep Dive:

YAML was first introduced in 2001 as a human-readable data serialization language. It is often compared to JSON, another popular data format, but YAML offers advantages such as better readability and the ability to include comments. Other alternatives such as XML and INI files may also be used for configuration data, but YAML has gained popularity for its simplicity and flexibility.

When working with YAML in C#, developers should be aware that YAML uses indentation to define data structures, much like Python. It is also important to check the syntax and structure of the YAML documents to ensure its readability and avoid parsing errors.

## See Also:

- [YAML spec](https://yaml.org/spec/)
- [YamlDotNet](https://github.com/aaubry/YamlDotNet)
- [SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Home)