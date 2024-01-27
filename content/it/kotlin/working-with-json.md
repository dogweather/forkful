---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON è un formato leggero per lo scambio di dati. Programmatori lo usano per la semplicità nella lettura e scrittura, e la facilità con cui i dati si convertono in strutture dati utilizzabili nel codice.

## How to:
```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class Utente(val nome: String, val eta: Int)

fun main() {
    // Serializzazione di un oggetto in JSON
    val utente = Utente("Francesco", 30)
    val jsonString = Json.encodeToString(Utente.serializer(), utente)
    println(jsonString) 

    // Deserializzazione JSON in oggetto
    val jsonStringInput = """{"nome":"Francesco","eta":30}"""
    val oggettoUtente = Json.decodeFromString(Utente.serializer(), jsonStringInput)
    println(oggettoUtente) 
}
```
Output:
```
{"nome":"Francesco","eta":30}
Utente(nome=Francesco, eta=30)
```

## Deep Dive
JSON, acronimo di JavaScript Object Notation, è stato proposto da Douglas Crockford agli inizi degli anni 2000. Alternativo a XML, è più snello e leggero. In Kotlin, le librerie più comuni per lavorare con JSON sono kotlinx.serialization e Moshi. kotlinx.serialization è strettamente integrata con il sistema di tipi Kotlin e supporta multiplatform, mentre Moshi è focalizzata su Android e interoperabilità con Java.

## See Also
- Documentazione Kotlin su JSON: https://kotlinlang.org/docs/serialization.html
- kotlinx.serialization GitHub: https://github.com/Kotlin/kotlinx.serialization
- Moshi GitHub: https://github.com/square/moshi
- Comparazione tra librerie JSON: https://www.baeldung.com/kotlin/json
