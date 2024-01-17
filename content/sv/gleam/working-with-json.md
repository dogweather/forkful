---
title:                "Arbeta med JSON"
html_title:           "Gleam: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) innebär att manipulera data i ett format som är lättläst för både människor och datorer. JSON används ofta för att överföra och lagra data, särskilt i webbutveckling. Det är populärt bland programmerare på grund av dess enkelhet och flexibilitet.

## Hur Man Gör:

Att arbeta med JSON i Gleam är enkelt och effektivt. För att kunna arbeta med JSON i Gleam måste man först importera modulen "json". Därefter kan man använda funktioner som "decode" och "encode" för att hantera JSON-data. Här är ett exempel som visar hur man kan dekodera JSON-data till en gleamtillströmning:

```Gleam
let decoded = json.decode(stream, decode_options)
```

Här är ett annat exempel som visar hur man kan koda en gleamvärde till JSON:

```Gleam
let encoded = json.encode(value)
```

För att använda dessa funktioner måste man också förstå hur man hanterar mönstermatchning och unions i Gleam.

## Djupdykning:

JSON har funnits sedan 2001 och har blivit ett populärt format för att överföra data på webben. Det finns också alternativ till JSON som XML och YAML, men JSON är ofta föredraget på grund av dess enkelhet och lätta vikt.

Att arbeta med JSON i Gleam möjliggörs tack vare alla funktioner och datavärder som finns tillgängliga. JSON i Gleam stödjer både omskrivning och fogning av JSON-data samt möjligheten att dekodera och kodera olika typer av värden till och från JSON-formatet. Detta gör det lättare att arbeta med komplexa datastrukturer och öppnar upp för mer avancerad datahantering.

## Se Även:

- Gleam json modul: https://gleam.run/modules/json/json/latest/
- JSON.org: https://www.json.org/json-en.html
- W3Schools JSON tutorial: https://www.w3schools.com/js/js_json_intro.asp