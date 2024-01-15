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

## Why
YAML (YAML Ain't Markup Language) is a popular data serialization format that is human-friendly and easy to read. It allows developers to store and transfer data in a structured way, making it a useful tool in various programming scenarios.

## How To
To work with YAML in C#, you will need to install the YamlDotNet library using NuGet. Once installed, you can start using it in your project. 

To start, create a `YamlStream` object and load a YAML file using the `YamlStream.Load()` method. 

```
using YamlDotNet.RepresentationModel;

YamlStream yaml = new YamlStream();
yaml.Load(File.OpenText("sample.yaml"));
```

Next, you can access the data stored in the YAML file by using the `RootNode` property of the `YamlStream` object. 

```
YamlMappingNode rootNode = (YamlMappingNode)yaml.RootNode;
```

You can then use the `Children` property to loop through the key-value pairs in the YAML file. 

```
foreach (var node in rootNode.Children)
{
    Console.WriteLine(node.Key + ": " + node.Value);
}
```

To write data to a YAML file, you can use the `Dump()` method of the `YamlStream` object. 

```
var data = new Dictionary<string, string>
{
    {"name", "John"},
    {"age", "30"}
};

var yamlData = new YamlStream();
yamlData.Add(new YamlMappingNode(data));
string output = yamlData.Dump();

Console.WriteLine(output);
```

The above code will output the following YAML string:

```
name: John
age: 30
```

## Deep Dive
YamlDotNet offers various helpful features such as serializing and deserializing custom objects, handling comments and multiple documents in a single YAML file, and more. 

To serialize objects into YAML, you can use the `Serializer` class. 

```
using YamlDotNet.Serialization;

var obj = new
{
    Name = "Mary",
    Age = 25
};

var serializer = new Serializer();
string yamlString = serializer.Serialize(obj);
```

The above code will output the following YAML string:

```
Name: Mary
Age: 25
```

To deserialize a YAML string into an object, you can use the `Deserializer` class. 

```
var deserializer = new Deserializer();
var obj = deserializer.Deserialize<YourClassName>(yamlString);
Console.WriteLine(obj.Name); // output: Mary
Console.WriteLine(obj.Age); // output: 25
```

For more information about working with YAML in C#, refer to the YamlDotNet documentation.

## See Also
- [YamlDotNet on GitHub](https://github.com/aaubry/YamlDotNet)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)