---
date: 2024-02-03 19:03:12.550269-07:00
description: "How to: C# provides the `System.Text.Json` namespace for efficient JSON\
  \ processing. To parse a JSON string to a C# object, define a class that matches\
  \ the\u2026"
lastmod: '2024-03-13T22:45:00.108721-06:00'
model: gpt-4-0125-preview
summary: C# provides the `System.Text.Json` namespace for efficient JSON processing.
title: Working with JSON
weight: 38
---

## How to:


### Parsing JSON String to an Object
C# provides the `System.Text.Json` namespace for efficient JSON processing. To parse a JSON string to a C# object, define a class that matches the JSON structure and use the `JsonSerializer.Deserialize` method.

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Name: John, Age: 30
    }
}
```

### Generating JSON from an Object
To convert a C# object back into a JSON string, use the `JsonSerializer.Serialize` method.

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // Output: {"Name":"Jane","Age":25}
    }
}
```

### Using Newtonsoft.Json
`Newtonsoft.Json` (or Json.NET) is a popular third-party library that offers more flexibility and options for JSON serialization and deserialization.

To use Json.NET, you must first install the `Newtonsoft.Json` package via NuGet. Then, you can deserialize a JSON string like so:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // Output: Name: Mike, Age: 22
    }
}
```

For generating JSON from an object with Json.NET:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // Output: {"Name":"Ella","Age":28}
    }
}
```

These snippets offer a quick start into handling JSON in C#, demonstrating both the built-in `System.Text.Json` capabilities and the extensive features of `Newtonsoft.Json`.
