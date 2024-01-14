---
title:                "C#: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

När det kommer till att arbeta med YAML i C# så finns det många fördelar. YAML är en format för att strukturera data i en textfil på ett enkelt och människovänligt sätt. Det är ofta användbart när man behöver lagra data som inte passar i traditionella databaser eller när man vill hålla data läsbar för människor.

## Hur man gör

Att jobba med YAML i C# är enkelt och smidigt. För att komma igång behöver man först installera ett lämpligt YAML-paket, till exempel det populära paketet YamlDotNet. Sedan kan man använda detta pakets funktioner för att läsa eller skriva YAML-filer.

```C#
using YamlDotNet.Serialization;
using System.IO;

var myObject = new { Name = "John", Age = 25 }; // Skapa en objekt med data

var serializer = new Serializer(); // Skapa en ny serializer

// Skicka objektet till en YAML-fil
using (var writer = new StreamWriter("file.yml"))
{
    serializer.Serialize(writer, myObject);
}

// Läs data från en YAML-fil
using (var reader = new StreamReader("file.yml"))
{
    var parsedObject = serializer.Deserialize(reader);
    Console.WriteLine(parsedObject.Name); // Skriver ut "John"
}
```

## Djupdykning

YAML kan vara användbart för en mängd olika ändamål, såsom att lagra konfigurationsfiler, läsa in och hantera API-data eller som en alternativ lagringsmetod i dina C#-applikationer. Det finns också en rad olika paket och verktyg som kan hjälpa till att göra arbetet med YAML ännu smidigare och mer dynamiskt.

See Also
- [YamlDotNet](https://github.com/aaubry/YamlDotNet)
- [YamlSerializer](https://github.com/antaris/YamlSerializer)
- [YAML-specifikationen](https://yaml.org/spec/1.2/spec.html)