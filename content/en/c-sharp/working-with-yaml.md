---
title:                "C# recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why 
Are you looking for a lightweight and human-readable way to store and share data between applications? Look no further than YAML! Whether you're a beginner or an experienced programmer, YAML (Yet Another Markup Language) is a great choice for storing configuration files, creating data-driven applications, and much more. So let's dive in and learn more about this versatile language.

## How To
To start working with YAML in C#, we first need to install a package called "YamlDotNet". Once that's installed, we can begin by creating a new YAML file and defining some data. For example, let's create a file called "sample.yml" with the following contents:

```
- name: John
  age: 30
  hobby:
    - hiking
    - cooking
- name: Sarah
  age: 25
  hobby:
    - reading
    - painting
```

In the above code, we have created a list of people with their names, ages, and hobbies. Now, let's see how we can access this data in our C# code:

```
using System;
using YamlDotNet.Serialization;
using System.IO;

public class Program
{
    public static void Main()
    {
        var deserializer = new DeserializerBuilder().Build();
        var yamlFile = File.OpenText("sample.yml");
        var data = deserializer.Deserialize<List<User>>(yamlFile);
        foreach(var user in data)
        {
            Console.WriteLine("Name: " + user.Name);
            Console.WriteLine("Age: " + user.Age);
            Console.WriteLine("Hobbies: " + string.Join(", ", user.Hobby));
            Console.WriteLine();
        }
    }
}

public class User
{
    public string Name { get; set; }
    public int Age { get; set; }
    public List<string> Hobby { get; set; }
}
```

Running this code will give us the following output:

```
Name: John
Age: 30
Hobbies: hiking, cooking

Name: Sarah
Age: 25
Hobbies: reading, painting
```

As you can see, we were able to easily access the data in our YAML file and use it in our code. This is just a simple example, but YAML can handle much more complex data structures as well.

## Deep Dive
Now that we have a basic understanding of how to work with YAML in C#, let's take a deeper look at some other features. One of the most useful features of YAML is its support for anchors and aliases. These allow us to reference the same data multiple times without having to repeat it. Let's see how this works with an example:

```
burger:
  - &meat
    type: beef
    quantity: 1
  - type: chicken
    quantity: 1
  - *meat
```

In the above code, we have created an anchor for the "beef" data and then referenced it later on with the alias "*meat". This can be particularly useful when working with large and complex data structures.

We can also use YAML to create more dynamic structures, such as arrays and dictionaries. For example, let's say we want to store a list of students with their corresponding grades in different subjects. We could do so using the following YAML code:

```
- name: John
  grades:
    math: 95
    science: 85
    history: 90
- name: Sarah
  grades:
    math: 85
    science: 90
    history: 95
```

This allows us to easily keep track of the different grades for each student and reference them by subject.

## See Also
- [YAML Specification](https://yaml.org/spec/)
- [YamlDotNet Documentation](https://github.com/aaubry/YamlDotNet/wiki)
- [Introduction to YAML for C# Developers](https://www.codeproject.com/Articles/1159125/Introduction-to-YAML-for-Csharp-Developers)