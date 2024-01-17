---
title:                "Arbeta med json"
html_title:           "C#: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Arbetet med JSON (JavaScript Object Notation) innebär att programmera för att hantera data i ett läsbart format som används för att överföra och lagra information. Det är ett populärt sätt för programmerare att hantera data på grund av dess enkelhet och universalitet.

## Så här gör du:

Här är ett exempel på hur du kan hantera JSON-data i C#:

```C#
string json = "{\"name\":\"Lisa\",\"age\":25}";
dynamic person = JsonConvert.DeserializeObject(json);
Console.WriteLine("Namn: " + person.name);
Console.WriteLine("Ålder: " + person.age);
```

Utmatningen av detta kodexempel blir:

```
Namn: Lisa
Ålder: 25
```
Notera att vi använder JsonConvert-klassen för att deserialisera JSON-datan till en dynamisk variabel, vilket gör det enkelt att komma åt datan med hjälp av dot notation.

## Djupdykning:

JSON har funnits sedan 2001 och har blivit en populär standard för dataöverföring. Det finns också andra format som används för samma ändamål som t.ex. XML, men JSON har blivit föredraget av många på grund av dess enkelhet och läsbarhet.

Det finns flera bibliotek för att arbeta med JSON i C#, inklusive Json.NET, som är det mest populära alternativet på grund av dess snabbhet och funktioner för att hantera olika datatyper.

## Se även:

- [Json.NET](https://www.newtonsoft.com/json)
- [JSON.org](https://www.json.org/json-en.html)