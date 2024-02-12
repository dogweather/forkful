---
title:                "Werken met JSON"
aliases:
- /nl/c-sharp/working-with-json/
date:                  2024-01-28T22:10:17.080361-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON betekent het verwerken en genereren van JSON (JavaScript Object Notation) data in uw applicaties. Programmeurs doen dit omdat JSON een lichtgewicht, tekstgebaseerd data-uitwisselingsformaat is dat eenvoudig te lezen en schrijven is voor mensen, en eenvoudig te verwerken en genereren is voor machines.

## Hoe:

In C# zult u waarschijnlijk de `System.Text.Json` namespace gebruiken voor JSON-verwerking. Stel dat u een eenvoudige klasse heeft:

```C#
public class Gamer
{
    public string GamerTag { get; set; }
    public int HighScore { get; set; }
}
```

Om dit object naar JSON te serialiseren, doe dit:

```C#
var gamer = new Gamer { GamerTag = "PlayerOne", HighScore = 9001 };
string jsonString = JsonSerializer.Serialize(gamer);
Console.WriteLine(jsonString);
```

Uitvoer:
```
{"GamerTag":"PlayerOne","HighScore":9001}
```

Om van JSON terug naar een object te deserialiseren:

```C#
string jsonString = "{\"GamerTag\":\"PlayerOne\",\"HighScore\":9001}";
Gamer gamer = JsonSerializer.Deserialize<Gamer>(jsonString);
Console.WriteLine($"GamerTag: {gamer.GamerTag}, HighScore: {gamer.HighScore}");
```

Uitvoer:
```
GamerTag: PlayerOne, HighScore: 9001
```

## Uitgebreid

JSON is sinds de vroege jaren 2000 het voorkeursdataformaat, het heeft XML overgenomen vanwege zijn eenvoud. Terwijl `System.Text.Json` nu de voorkeursbibliotheek in C# is voor .NET Core en .NET 5+, was de `Newtonsoft.Json` bibliotheek vele jaren de de facto standaard. `System.Text.Json` richt zich op hoge prestaties en lage geheugentoewijzing, maar `Newtonsoft.Json` heeft nog steeds een bredere functieset die sommige applicaties nodig kunnen hebben.

## Zie Ook

- Microsoft Docs over `System.Text.Json`: https://docs.microsoft.com/dotnet/standard/serialization/system-text-json-overview
- Newtonsoft.Json (Json.NET): https://www.newtonsoft.com/json
- JSON Specificatie: https://www.json.org/json-nl.html
