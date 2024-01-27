---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML hantering i C# innebär att tolka eller generera YAML, ett enkelt format för dataseriering. Programmerare använder det för konfigfiler och datautbyte då det är både människo- och maskinläsbart.

## How to:
För att jobba med YAML i C# behöver du ett bibliotek som YamlDotNet. Här är grundläggande exempel för att läsa och skriva YAML.

Först, installera YamlDotNet via NuGet:
```csharp
PM> Install-Package YamlDotNet
```

Läs YAML:
```csharp
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;
using System.IO;

var yamlInput = @"
name: C# Programmer
age: 30
languages:
  - Swedish
  - English
";

var deserializer = new DeserializerBuilder()
  .WithNamingConvention(CamelCaseNamingConvention.Instance)  // Använder camelCase
  .Build();

var programmer = deserializer.Deserialize<Programmer>(new StringReader(yamlInput));

Console.WriteLine(programmer.Name); // Output: C# Programmer
```

Skriva YAML:
```csharp
var serializer = new SerializerBuilder()
  .WithNamingConvention(CamelCaseNamingConvention.Instance)  // Använder camelCase
  .Build();

var programmer = new Programmer
{
    Name = "C# Programmer",
    Age = 30,
    Languages = new List<string> { "Swedish", "English" }
};

string yamlOutput = serializer.Serialize(programmer);
Console.WriteLine(yamlOutput);
```

## Deep Dive
YAML, "YAML Ain't Markup Language", skapades 2001 och är ett alternativ till XML och JSON. I .NET-världen är [YamlDotNet](https://github.com/aaubry/YamlDotNet) det mest populära biblioteket. YamlDotNet använder deserialisering och serialisering liknande JSON.NET, det gör det lätt för C#-utvecklare att hoppar in.

Andra alternativ inkluderar `System.Text.Json` och `Newtonsoft.Json` för JSON, eller `System.Xml` för XML. YAML är populär i DevOps för dess läsbarhet, speciellt i Docker och Kubernetes konfigurationer.

## See Also
- YamlDotNet GitHub Repo: https://github.com/aaubry/YamlDotNet
- Officiell YAML-webbsida: https://yaml.org
- Microsofts guide till JSON i .NET: https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-overview
