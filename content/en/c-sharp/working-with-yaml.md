---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML is a human-friendly data serialization standard used in programming for config files, data storage, and more. Programmers use it for its readability and simplicity in complex applications and systems.

## How to:
To work with YAML in C#, you'll need the YamlDotNet library. You can install it via NuGet: `Install-Package YamlDotNet`.

First, let's serialize an object to a YAML string:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}

class Program {
    static void Main(string[] args) {
        var person = new Person {
            FirstName = "Jamie",
            LastName = "Smith",
            Age = 35
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        string yaml = serializer.Serialize(person);
        Console.WriteLine(yaml);
    }
}
```

Output:
```yaml
firstName: Jamie
lastName: Smith
age: 35
```

Next, let's read a YAML file and deserialize it:

```C#
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program {
    static void Main(string[] args) {
        var yaml = @"
firstName: Jamie
lastName: Smith
age: 35
";
        
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();
        
        Person person = deserializer.Deserialize<Person>(yaml);
        
        Console.WriteLine($"Hello, {person.FirstName} {person.LastName}!");
    }
}

public class Person {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public int Age { get; set; }
}
```

Output:
```
Hello, Jamie Smith!
```

## Deep Dive
YAML, meaning "YAML Ain't Markup Language," was first proposed in 2001 to be more human-readable than XML. It's widely used in DevOps for CI/CD pipeline configurations, like in Docker Compose files or Kubernetes deployment manifests. JSON is a YAML superset, meaning JSON files are also valid YAML. Implementation-wise, parsing YAML in C# requires a library like YamlDotNet because there's no native support.

## See Also
- [YamlDotNet GitHub Repository](https://github.com/aaubry/YamlDotNet)
- [Official YAML website](https://yaml.org)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)