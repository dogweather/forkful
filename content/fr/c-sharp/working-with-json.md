---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
**Quoi et Pourquoi ?**

Manipuler JSON, c'est jongler avec des données textuelles formatées pour échanger des infos entre système. Les devs le font car c'est léger, lisible par l’humain et adoré des API et configs.

## How to:
**Comment faire :**

Installez `System.Text.Json` via NuGet. Voici comment parser du JSON :

```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"name\":\"Jean\",\"age\":30}";
        var person = JsonSerializer.Deserialize<Person>(jsonString);
        Console.WriteLine($"Nom: {person.Name}, Âge: {person.Age}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}
```
Sortie:
```
Nom: Jean, Âge: 30
```
Pour créer du JSON :
```C#
Person person = new Person { Name = "Jean", Age = 30 };
string jsonString = JsonSerializer.Serialize(person);
Console.WriteLine(jsonString);
```
Sortie :
```
{"Name":"Jean","Age":30}
```

## Deep Dive:
**Plongée Profonde :**

JSON, pour JavaScript Object Notation, a été introduit par Douglas Crockford. Alternatives incluent XML, mais JSON prime en web pour sa simplicité. En C#, `System.Text.Json` est rapide et efficace contre `Newtonsoft.Json` qui offre plus de fonctionnalités mais peut être plus lourd.

## See Also:
**Voir Aussi :**

- [Documentation officielle de `System.Text.Json`](https://docs.microsoft.com/fr-fr/dotnet/standard/serialization/system-text-json-overview)
