---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, acronimo di "YAML Ain't Markup Language", è un formato di serializzazione leggibile dall'uomo, utilizzato per configurazioni, file di dati e in ambienti di sviluppo. I programmatori scelgono YAML per la sua chiarezza e la facilità di integrazione con vari linguaggi, tra cui C#.

## How to:
Installiamo prima il pacchetto NuGet `YamlDotNet` che ci permette di lavorare con YAML in C#.

```csharp
// Installare via NuGet Console:
// Install-Package YamlDotNet
```

Creiamo una classe di esempio e serializziamola in YAML:

```csharp
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var person = new Person { Name = "Mario", Age = 30, Email = "mario@email.com" };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var yaml = serializer.Serialize(person);
        Console.WriteLine(yaml);
    }
}

public class Person 
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Email { get; set; }
}
```

Output:

```
name: Mario
age: 30
email: mario@email.com
```

E ora deserializziamo da YAML a un oggetto C#:

```csharp
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class Program
{
    public static void Main()
    {
        var yaml = @"
name: Luigi
age: 25
email: luigi@email.com
";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .Build();

        var person = deserializer.Deserialize<Person>(yaml);
        Console.WriteLine($"Nome: {person.Name}, Età: {person.Age}, Email: {person.Email}");
    }
}

public class Person 
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Email { get; set; }
}
```

Output:

```
Nome: Luigi, Età: 25, Email: luigi@email.com
```

## Deep Dive
YAML è nato all'inizio degli anni 2000 come alternativa a XML. Elementi come l'indentazione al posto delle parentesi e l'assenza di tag di chiusura rendono YAML particolarmente leggibile. Per quanto riguarda le alternative, JSON è il principale in termini di semplicità e compatibilità con JavaScript. Per implementare YAML in C#, `YamlDotNet` è una delle librerie più consolidate ed è facilmente estensibile per soddisfare casi d'uso più complessi.

## See Also
- YAML Homepage: https://yaml.org
- `YamlDotNet` su GitHub: https://github.com/aaubry/YamlDotNet
- Documentazione di `YamlDotNet`: https://aaubry.net/pages/yamldotnet.html
- Specifiche YAML 1.2: https://yaml.org/spec/1.2/spec.html
