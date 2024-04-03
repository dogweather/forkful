---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:50.699299-07:00
description: "\u041A\u0430\u043A: \u0412 C# \u0434\u043B\u044F \u043E\u0431\u0440\u0430\
  \u0431\u043E\u0442\u043A\u0438 JSON \u0441\u043A\u043E\u0440\u0435\u0435 \u0432\u0441\
  \u0435\u0433\u043E \u0431\u0443\u0434\u0435\u0442 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C\u0441\u044F \u043F\u0440\u043E\u0441\u0442\
  \u0440\u0430\u043D\u0441\u0442\u0432\u043E \u0438\u043C\u0435\u043D `System.Text.Json`.\
  \ \u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, \u0443 \u0432\u0430\u0441 \u0435\
  \u0441\u0442\u044C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043A\u043B\u0430\
  \u0441\u0441."
lastmod: '2024-03-13T22:44:45.095494-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C# \u0434\u043B\u044F \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\
  \u0438 JSON \u0441\u043A\u043E\u0440\u0435\u0435 \u0432\u0441\u0435\u0433\u043E\
  \ \u0431\u0443\u0434\u0435\u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u044C\u0441\u044F \u043F\u0440\u043E\u0441\u0442\u0440\u0430\u043D\
  \u0441\u0442\u0432\u043E \u0438\u043C\u0435\u043D `System.Text.Json`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
