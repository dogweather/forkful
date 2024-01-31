---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:50.699299-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Работа с JSON включает в себя разбор и генерацию данных JSON (JavaScript Object Notation) в ваших приложениях. Программисты делают это, потому что JSON является легковесным, текстовым форматом обмена данными, который легко читать и писать людям, а также легко разбирать и генерировать машинами.

## Как:

В C# для обработки JSON скорее всего будет использоваться пространство имен `System.Text.Json`. Допустим, у вас есть простой класс:

```C#
public class Gamer
{
    public string GamerTag { get; set; }
    public int HighScore { get; set; }
}
```

Чтобы сериализовать этот объект в JSON, делайте следующее:

```C#
var gamer = new Gamer { GamerTag = "PlayerOne", HighScore = 9001 };
string jsonString = JsonSerializer.Serialize(gamer);
Console.WriteLine(jsonString);
```

Вывод:
```
{"GamerTag":"PlayerOne","HighScore":9001}
```

Чтобы десериализовать из JSON обратно в объект:

```C#
string jsonString = "{\"GamerTag\":\"PlayerOne\",\"HighScore\":9001}";
Gamer gamer = JsonSerializer.Deserialize<Gamer>(jsonString);
Console.WriteLine($"GamerTag: {gamer.GamerTag}, HighScore: {gamer.HighScore}");
```

Вывод:
```
GamerTag: PlayerOne, HighScore: 9001
```

## Подробнее

JSON является предпочтительным форматом данных с начала 2000-х годов, вытеснив XML благодаря своей простоте. Хотя сейчас в C# для .NET Core и .NET 5+ предпочтительной библиотекой является `System.Text.Json`, библиотека `Newtonsoft.Json` многие годы была фактическим стандартом. `System.Text.Json` сосредоточена на высокой производительности и низком расходе памяти, но `Newtonsoft.Json` по-прежнему обладает более широким набором функций, который может потребоваться в некоторых приложениях.

## См. также

- Документация Microsoft по `System.Text.Json`: https://docs.microsoft.com/dotnet/standard/serialization/system-text-json-overview
- Newtonsoft.Json (Json.NET): https://www.newtonsoft.com/json
- Спецификация JSON: https://www.json.org/json-en.html
