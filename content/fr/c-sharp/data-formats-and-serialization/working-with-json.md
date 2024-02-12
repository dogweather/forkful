---
title:                "Travailler avec JSON"
aliases: - /fr/c-sharp/working-with-json.md
date:                  2024-02-03T19:22:08.675285-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec le JSON (JavaScript Object Notation) implique l'analyse, la génération et l'interrogation de données JSON, ce qui en fait une compétence essentielle pour la programmation moderne. Ce format d'échange de données est extrêmement utilisé dans les services web et les API en raison de sa lisibilité facile et de son indépendance linguistique, le rendant indispensable pour les programmeurs C# travaillant sur des applications en réseau ou interagissant avec des données basées sur le web.

## Comment faire :

### Analyser une chaîne JSON en un objet

C# fournit l'espace de noms `System.Text.Json` pour un traitement JSON efficace. Pour analyser une chaîne JSON en un objet C#, définissez une classe qui correspond à la structure JSON et utilisez la méthode `JsonSerializer.Deserialize`.

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
        // Sortie : Name: John, Age: 30
    }
}
```

### Générer du JSON à partir d'un objet

Pour convertir un objet C# en une chaîne JSON, utilisez la méthode `JsonSerializer.Serialize`.

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
        // Sortie : {"Name":"Jane","Age":25}
    }
}
```

### Utilisation de Newtonsoft.Json

`Newtonsoft.Json` (ou Json.NET) est une librairie tierce populaire qui offre plus de flexibilité et d'options pour la sérialisation et la désérialisation JSON.

Pour utiliser Json.NET, vous devez d'abord installer le paquet `Newtonsoft.Json` via NuGet. Ensuite, vous pouvez désérialiser une chaîne JSON comme suit :

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
        // Sortie : Name: Mike, Age: 22
    }
}
```

Pour générer du JSON à partir d'un objet avec Json.NET :

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
        // Sortie : {"Name":"Ella","Age":28}
    }
}
```

Ces extraits offrent un démarrage rapide pour la manipulation du JSON en C#, démontrant à la fois les capacités intégrées de `System.Text.Json` et les fonctionnalités étendues de `Newtonsoft.Json`.
