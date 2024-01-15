---
title:                "Att arbeta med yaml"
html_title:           "C#: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför
YAML, som står för "YAML Ain't Markup Language", är en textbaserad dataformat som är enkelt att läsa och skriva för både människor och datorer. Det används oftast för att konfigurera program eller applikationer. Att kunna arbeta med YAML är en viktig färdighet för utvecklare eftersom det är ett vanligt format för att dela och lagra data.

## Så här gör du
Här är några praktiska exempel på hur man kan använda YAML i C#.

#### Skapa en YAML-fil
För att skapa en YAML-fil kan du använda StreamWriter från System.IO:

```C#
using (StreamWriter writer = new StreamWriter("minfil.yml"))
{
    // Skriv innehåll till filen
    writer.WriteLine("version: 1.0.0");
    writer.WriteLine("author: John Doe");
    writer.WriteLine("description: En exempelfil i YAML-format");
}
```

#### Läs en YAML-fil
För att läsa innehållet från en YAML-fil kan du använda YamlDotNet, som är ett populärt bibliotek för att arbeta med YAML i C#. Installera det genom att använda NuGet Package Manager i Visual Studio eller genom att köra kommandot "Install-Package YamlDotNet" i nuGet Package Manager Console.

```C#
// Skapa en YamlReader
using (var reader = new StreamReader("minfil.yml"))
{
    var deserializer = new DeserializerBuilder().Build();
    // Läs in innehållet och deserialisera till en Dictionary
    var yamlObject = deserializer.Deserialize<Dictionary<string, string>>(reader);
    // Hämta värden från Dictionary
    string version = yamlObject["version"];
    string author = yamlObject["author"];
    string description = yamlObject["description"];
    // Visa resultaten
    Console.WriteLine("Version: " + version);
    Console.WriteLine("Author: " + author);
    Console.WriteLine("Description: " + description);
}
```

#### Arbeta med YAML i ett C#-projekt
För att kunna arbeta med YAML i C#-projekt behöver du installera paketet YamlDotNet som nämnts tidigare. Du kan sedan använda dotnet command line för att lägga till ett nytt paket eller genom att högerklicka på ditt projekt i Visual Studio och välja "Manage NuGet Packages".

## Deep Dive
Här är några tips och tricks för att arbeta med YAML i C#.

- YAML stödjer olika datatyper, såsom strängar, arrayer, numeriska värden och mer. Se till att använda rätt datatyp när du definierar värden i din YAML-fil för att undvika problem vid läsning.
- För att skapa en array i YAML, använd en lista med ett bindestreck (-) före varje element.
- Du kan använda punkter för att skapa hierarkiska strukturer i YAML.
- YamlDotNet har en mängd olika funktioner för att hantera komplexa datastrukturer i YAML-filer, såsom åtkomst till element baserat på nycklar och ignorering av okända nycklar vid deserialisering.

## Se även
- YamlDotNet Official Website: https://github.com/aaubry/YamlDotNet
- YAML Specification: https://yaml.org/spec/1.2/spec.html