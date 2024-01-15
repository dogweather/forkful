---
title:                "yaml के साथ काम करना"
html_title:           "C#: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Kyu
YAML ek plain text data format hai jo configuration files aur data serialization ke liye istemal kiya jata hai. Yah ek simple aur readable syntax provide karta hai jise humans bhi aasani se samajh sakte hai. C# mein YAML ka use karne se aap apne code ko clean aur organized rakh sakte hai.

## Kaise Kare
YAML ko C# mein use karne ke liye, aapko 'YamlDotNet' library install karni hogi. Uske baad, aap 'YamlDotNet' namespace ko apne code mein import kar sakte hai. Fir aap 'YamlStream' object ka use karke YAML file ko read aur write kar sakte hai. Niche diye gaye code snippets mein ek basic example diya gaya hai.

```C#
// Example YAML:
// name: John Doe
// age: 25

// Reading YAML:
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Core;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        string yaml = "name: John Doe\nage: 25";
        var deserializer = new DeserializerBuilder().Build();
        var person = deserializer.Deserialize<Person>(yaml);
        Console.WriteLine(person.Name); // Output: John Doe
        Console.WriteLine(person.Age); // Output: 25
    }
    
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }
}

// Writing YAML:
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var person = new Person { Name = "John Doe", Age = 25 };
        var serializer = new SerializerBuilder().Build();
        var yaml = serializer.Serialize(person);
        Console.WriteLine(yaml); // Output: name: John Doe\nage: 25
    }
    
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }
}
```

## Deep Dive
YAML mein strings, numbers, lists, objects aur comments ka use kiya ja sakta hai. Iske alawa, YAML mein ek important concept hai 'anchors'. Anchors kisi specific value ko assign karne ke liye use kiye jate hai taki use later mein kisi aur value mein refer kiya ja sake. Iske alawa, 'tags' bhi use kiye jate hai jisse ek specific data type ko indicate kiya ja sake. In sab concepts ko samajhna aur use karna YAML ka deeper dive hai.

## See Also
Apne YAML knowledge ko aur improve karne ke liye, neeche diye gaye links ka istemal kar sakte hai:
- [The Official YAML Website](https://yaml.org/)
- [YAML Tutorial by Tutorialspoint](https://www.tutorialspoint.com/yaml/index.htm)
- [YAML Syntax Cheatsheet by QuickRef.me](https://quickref.me/yaml)