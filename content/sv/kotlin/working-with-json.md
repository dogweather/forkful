---
title:                "Kotlin: Att arbeta med JSON"
simple_title:         "Att arbeta med JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
I vår digitala värld är JSON (JavaScript Object Notation) en vanligt förekommande standard för att hantera data och information. Genom att lära sig hur man arbetar med JSON kommer du kunna skriva kod som kan läsa och skriva data från och till externa källor, vilket är en viktig färdighet för varje utvecklare inom dagens tekniska landskap.

## Hur man gör
Att arbeta med JSON i Kotlin är enkelt och smidigt. Du behöver endast använda den inbyggda klassen `JSONObject`, som tillåter dig att skapa och hantera JSON-objekt på ett snyggt sätt. Här är ett exempel på hur du kan skapa ett JSON-objekt med hjälp av `JSONObject`:

```Kotlin
val person = JSONObject()
person.put("name", "Johan")
person.put("age", 25)
person.put("city", "Stockholm")

println(person.toString())
```

Output:
```
{"name":"Johan","age":25,"city":"Stockholm"}
```

För att läsa data från en JSON-sträng kan vi använda metoden `get()` och ange namnet på attributet vi vill hämta data från. Till exempel:

```Kotlin
val person = JSONObject("{\"name\":\"Johan\",\"age\":25,\"city\":\"Stockholm\"}")

println("Name: " + person.get("name"))
println("Age: " + person.get("age"))
println("City: " + person.get("city"))
```

Output:
```
Name: Johan
Age: 25
City: Stockholm
```

Du kan också sätta ihop komplexa JSON-strängar genom att använda `JSONObject` tillsammans med `JSONArray`, som låter dig skapa listor av objekt inuti ditt JSON-objekt. Här är ett exempel på hur det skulle kunna se ut:

```Kotlin
val person1 = JSONObject()
person1.put("name", "Eva")
person1.put("age", 30)
person1.put("city", "Göteborg")

val person2 = JSONObject()
person2.put("name", "Lars")
person2.put("age", 35)
person2.put("city", "Malmö")

val personList = JSONArray()
personList.put(person1)
personList.put(person2)

val allPersons = JSONObject()
allPersons.put("count", 2)
allPersons.put("persons", personList)

println(allPersons.toString())
```

Output:
```
{"count":2,"persons":[{"name":"Eva","age":30,"city":"Göteborg"},{"name":"Lars","age":35,"city":"Malmö"}]}
```

## Djupdykning
Utöver skapandet och hanteringen av grundläggande JSON-strängar finns det många andra sätt att arbeta med JSON i Kotlin. Till exempel kan man använda sig av tredjepartsbibliotek som Gson eller Jackson för att enklare hantera komplexa JSON-datastrukturer. Det finns även möjligheter att göra asynkrona anrop till externa API:er för att hämta och skicka JSON-data.

En annan viktig aspekt att ta hänsyn till när man arbetar med JSON är att vara medveten om vilken typ av data man förväntar sig att få tillbaka från en extern källa. Ofta kan källor skicka olika typer av data som t.ex. strängar, nummer eller boolean-värden, vilket kan påverka hur man hanterar datan i sin kod.

## Se även
- [Kotlin JSON dokumentation](https://kotlinlang.org/docs/reference/js-interop.html)
- [Gson biblioteket](https://github.com/google/gson)
- [Jackson biblioteket](https://github.com/FasterXML/jackson)