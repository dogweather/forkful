---
title:                "Gleam: Att arbeta med JSON"
simple_title:         "Att arbeta med JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-json.md"
---

{{< edit_this_page >}}

# Varför Arbeta Med JSON i Gleam?

JavaScript Object Notation (JSON) är ett populärt format för datautbyte, vilket gör det till en viktig del av webbutveckling och datahantering. I Gleam programmeringsspråk, är det relativt enkelt att arbeta med JSON och det finns flera fördelar med att använda det. Detta inlägg kommer att förklara varför det är värt att lära sig att arbeta med JSON i Gleam.

## Hur Man Gör

För att arbeta med JSON i Gleam behöver du först importera modulen `json` genom att lägga till `import json` överst i din kodfil. Sedan kan du skapa en JSON-sträng genom att använda funktionen `json.encode(data)`. Till exempel:

```Gleam
import json

let data = {
  name: "John Doe",
  age: 30,
  hobbies: ["gaming", "reading", "coding"]
}

let json_string = json.encode(data)
```

Detta kommer att skapa en giltig JSON-sträng från datan vi tillhandahöll. Du kan också göra motsatsen genom att använda funktionen `json.decode(string)` som tar en JSON-sträng och konverterar den till Gleams typsystem. Exempel:

```Gleam
import json

let json_string = "{\"name\": \"John Doe\", \"age\": 30, \"hobbies\": [\"gaming\", \"reading\", \"coding\"]}"

let data = json.decode(json_string)
```

Detta kommer att ge oss en typ `Result(Error, HashMap(String, Any))`, där `Any` är en generisk typ som kan representera vilken typ av data som helst. Du kan sedan använda `Result.map` för att hantera datan på ett säkert sätt.

## Djupdykning

När du arbetar med JSON i Gleam, kommer du att stöta på olika funktioner som `json.parse(string)` och `json.prettify(json_string)`. `json.parse` används för att konvertera en JSON-sträng till ett Gleam-värde, medan `json.prettify` används för att formatera en JSON-sträng för läsbarhet. I modulen `json`, finns det också funktioner som `json.skip_nulls` och `json.encoding` som är användbara för att hantera specifika egenskaper i din JSON-data.

När du arbetar med större och mer komplexa JSON-strängar, kan det vara användbart att använda externa bibliotek för att underlätta hanteringen. Gleam har bra stöd för externa bibliotek och det finns flera alternativ för att arbeta med JSON, som till exempel `gleam_json` och `gleam-serialization`.

## Se Också

- [Gleam - Officiell webbplats](https://gleam.run/)
- [Gleam - Officiell dokumentation](https://gleam.run/documentation/)
- [Gleam JSON-modul - Officiell dokumentation](https://gleam.run/modules/json.html)
- [Gleam JSON-modul på GitHub](https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/json.gleam)
- [Gleam JSON-modul på Hex](https://hex.pm/packages/gleam_json)