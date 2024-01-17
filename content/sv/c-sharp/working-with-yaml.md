---
title:                "Att jobba med yaml"
html_title:           "C#: Att jobba med yaml"
simple_title:         "Att jobba med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

YAML är en strukturerad dataformat som ofta används av programmerare för att läsa, skriva och lagra data. Det är ett enkelt sätt att organisera och strukturera information på ett läs- och förståeligt sätt. Många föredrar att använda YAML istället för XML eller JSON eftersom det är mer läsbar och lättare att lära sig.

## Hur man:

För att använda YAML i C# behöver du först installera ett YAML-bibliotek, som exempelvis YamlDotNet. Sedan kan du använda olika metoder beroende på vilken typ av data du vill läsa eller skriva.

```C#
// Läs en YAML-fil
string yamlString = File.ReadAllText("exempel.yml"); // Läs filen som en sträng
var deserializer = new DeserializerBuilder().Build(); // Skapa en Deserializer
var yamlObject = deserializer.Deserialize(yamlString); // Skapa ett C# objekt från YAML-strängen

// Skriv till en YAML-fil
var serializer = new SerializerBuilder().Build(); // Skapa en Serializer
string yamlOutput = serializer.Serialize(c#Object); // Skapa en YAML-sträng från C# objektet
File.WriteAllText("exempel.yml", yamlOutput); // Skriv YAML-strängen till en fil
```

## Djupdykning:

YAML lanserades 2001 och är en förkortning för "YAML Ain't Markup Language". Det är ett human-readable dataformat som används för att representera strukturerad data. I C# finns flera olika bibliotek som stödjer YAML, som exempelvis SharpYAML och YamlDotNet.

Jämfört med andra dataformat som XML och JSON, är YAML mer lättläst för människor men inte lika effektivt för datorer. YAML är också mindre strikt i sin syntax, vilket kan leda till öppna dörrar för felaktig formatering.

## Se också:

- Officiell YAML hemsida: https://yaml.org/
- YamlDotNet dokumentation: https://dotnetyaml.readthedocs.io
- SharpYAML på GitHub: https://github.com/xoofx/SharpYaml