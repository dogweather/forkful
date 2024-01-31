---
title:                "Working with JSON"
date:                  2024-01-19
simple_title:         "Working with JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON means parsing and generating JSON (JavaScript Object Notation) data in your applications. Programmers do it because JSON is a lightweight, text-based data interchange format that's easy for humans to read and write, and easy for machines to parse and generate.

## How to:

In C#, you'll likely use the `System.Text.Json` namespace for JSON processing. Let's say you've got a simple class:

```C#
public class Gamer
{
    public string GamerTag { get; set; }
    public int HighScore { get; set; }
}
```

To serialize this object to JSON, do this:

```C#
var gamer = new Gamer { GamerTag = "PlayerOne", HighScore = 9001 };
string jsonString = JsonSerializer.Serialize(gamer);
Console.WriteLine(jsonString);
```

Output:
```
{"GamerTag":"PlayerOne","HighScore":9001}
```

To deserialize from JSON back into an object:

```C#
string jsonString = "{\"GamerTag\":\"PlayerOne\",\"HighScore\":9001}";
Gamer gamer = JsonSerializer.Deserialize<Gamer>(jsonString);
Console.WriteLine($"GamerTag: {gamer.GamerTag}, HighScore: {gamer.HighScore}");
```

Output:
```
GamerTag: PlayerOne, HighScore: 9001
```

## Deep Dive

JSON has been the go-to data format since the early 2000s, taking over from XML due to its simplicity. While `System.Text.Json` is now the preferred library in C# for .NET Core and .NET 5+, the `Newtonsoft.Json` library was the de facto standard for many years. `System.Text.Json` focuses on high performance and low memory allocation, but `Newtonsoft.Json` still has a broader feature set that some applications may require.

## See Also

- Microsoft Docs on `System.Text.Json`: https://docs.microsoft.com/dotnet/standard/serialization/system-text-json-overview
- Newtonsoft.Json (Json.NET): https://www.newtonsoft.com/json
- JSON Specification: https://www.json.org/json-en.html
