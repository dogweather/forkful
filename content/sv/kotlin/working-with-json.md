---
title:                "Arbeta med json"
html_title:           "Kotlin: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att arbeta med JSON är ett sätt för programmerare att hantera och utbyta data på ett enkelt och strukturerat sätt. JSON används ofta för att spara och överföra data mellan olika applikationer eller system.

# Så här gör du:
```Kotlin 
// Skapa en ny JSON-object
val jsonObject = JsonObject()

// Lägg till värden i objektet
jsonObject.addProperty("id", 123)
jsonObject.addProperty("name", "Kotlin programmering")
jsonObject.addProperty("author", "Jane Doe")

// Konvertera till JSON-sträng
val jsonString = jsonObject.toString()

// Skriv ut resultatet
println(jsonString)
```

Resultat:
```Kotlin
{"id":123,"name":"Kotlin programmering","author":"Jane Doe"}
```

# Djupdykning:
JSON (JavaScript Object Notation) har funnits sedan 90-talet och är ett populärt format för att strukturera data inom webbapplikationer. En av fördelarna med JSON är att den är läsbar för både människor och datorer. Andra alternativ för att hantera strukturerad data är till exempel XML och CSV, men JSON är oftast mer lättförståeligt och enklare att använda.

För att arbeta med JSON i Kotlin finns det flera bibliotek att välja mellan, till exempel Jackson, Gson, och Moshi. Dessa bibliotek erbjuder enkel konvertering mellan JSON och Kotlin-objekt.

# Se även:
- [JSON](https://www.json.org/)
- [Kotlin Docs: JSON](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-pretty-json.html)
- [JSON and Kotlin: A Perfect Match](https://www.baeldung.com/kotlin-json)