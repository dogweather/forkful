---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON står för JavaScript Object Notation och används för att lagra och utbyta data. Programmerare använder JSON för att det är textbaserat, lättläst och språköverskridande, vilket gör det optimalt för datakommunikation på webben.

## Hur gör man:
I Kotlin kan vi hantera JSON enkelt med bibliotek som kotlinx.serialization eller Gson. Här är ett grundläggande exempel med kotlinx.serialization:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json.encodeToString(User("Anna", 25))
    println(json)

    val user = Json.decodeFromString<User>(json)
    println(user)
}
```
Utdata:
```
{"name":"Anna","age":25}
User(name=Anna, age=25)
```

## Djupdykning
JSON härstammar från JavaScript men har blivit ett standardformat för webbaserad datautbyte. Kotlin erbjuder flera bibliotek för JSON: kotlinx.serialization är officiellt stödd av Kotlin, medan Gson är ett populärt alternativ. När du implementerar JSON-parsing, överväg prestanda, flexibilitet och underhåll av de olika biblioteken.

## Se också
- Kotlinx.serialization GitHub: https://github.com/Kotlin/kotlinx.serialization
- Gson GitHub: https://github.com/google/gson
- JSON officiell webbplats: https://www.json.org/json-en.html
