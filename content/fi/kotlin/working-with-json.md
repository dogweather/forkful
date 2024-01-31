---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON on datanvaihtomuoto, joka on kevyt ja selkeä. Käytämme sitä datan siirtämiseen ja tallentamiseen eri ohjelmien ja palvelimien välillä.

## How to:
```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// JSON-muotoilun määrittely
@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Luodaan User-olio
    val user = User("Mikko", 30)
    
    // Olio JSON-stringiksi
    val jsonString = Json.encodeToString(user)
    println(jsonString) // {"name":"Mikko","age":30}

    // JSON-stringi olioksi
    val userObj = Json.decodeFromString<User>(jsonString)
    println(userObj) // User(name=Mikko, age=30)
}
```

## Deep Dive
JSON, lyhenne sanoista JavaScript Object Notation, syntyi 2000-luvun alussa JavaScriptin osana, mutta sitä käytetään nykyään kieliriippumattomasti. Alternatiiveina on esimerkiksi XML, mutta JSON on noussut suosioon sen keveyden ja ymmärrettävyyden ansiosta. Kotliinin `kotlinx.serialization` on moderni tapa käsitellä JSON-dataa: se on tehokkaampi verrattuna vanhempiin kirjastoihin kuten Gson ja Jackson.

## See Also
- Kotliinin viralliset `kotlinx.serialization`-dokumentit: [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- JSON-tietoa ja -työkaluja: [json.org](https://www.json.org/json-en.html)
- Verrataan JSONiä XML:ään: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
