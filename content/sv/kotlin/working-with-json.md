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

## Varför

Har du någonsin behövt arbeta med data i JSON-format? Det är en vanlig filtyp för att lagra och överföra data över webben, och det är också lättläst både för människor och maskiner.

## Så här gör du

Att arbeta med JSON i Kotlin är snabbt och enkelt. Först behöver vi importera biblioteket "kotlinx.serialization" så att vi kan använda "Json" -objektet.

```kotlin
import kotlinx.serialization.json.Json
```

För att skapa ett nytt JSON-objekt behöver vi bara använda Json-konstruktorn och ange vår data som en sträng.

```kotlin
val jsonString = """ 
                    {
                        "name": "John",
                        "age": 30,
                        "hobbies": ["painting", "hiking"]
                    } 
                  """
val json = Json.parse(jsonString)
```

Vi kan nu använda json-objektet för att få tillgång till vår data. Med hjälp av ".string" eller ".int" kan vi få tag på enskilda datatyper, medan ".getList" låter oss få en lista på ett visst fält.

```kotlin
val name = json.string("name") // "John"
val age = json.int("age") // 30
val hobbies = json.getList("hobbies") // ["painting", "hiking"]
```

Men vad händer om vår JSON-fil innehåller ett stort antal fält och vi vill bara få tag på vissa av dem? Det är där ".getJsonObject" kommer till nytta. Vi kan använda den för att få ett under-JSON-objekt som innehåller de fält vi behöver.

```kotlin
val ladyJson = json.getJsonObject("lady") // Skapar ett nytt under-JSON-objekt
val name = ladyJson.string("name") // "Linda"
val age = ladyJson.string("age") // 28
```

## Djupdyka

Kotlin har en inbyggd JSON-parser som använder Kotlinx.serialization-biblioteket. Det ger oss enkelhet och effektivitet i vår kod. Men beroende på vår applikation och hur mycket data vi behöver hantera kan det hända att vi behöver använda andra bibliotek som har mer avancerade funktioner.

Ett annat viktigt koncept att förstå när man arbetar med JSON i Kotlin är hur man hanterar noll-värden. I JSON är det möjligt att ha fält som inte har något värde, och det kan leda till problem om vi inte hanterar dem på rätt sätt. I Kotlin kan vi använda "?:"-operatorn för att undvika NullPointerExceptions när vi arbetar med noll-värden.

## Se även

- JSON För Nybörjare: https://kotlinlang.org/docs/tutorials/kotlin-for-py/introduction.html
- Kotlinx.serialization Dokumentation: https://github.com/Kotlin/kotlinx.serialization