---
title:                "Lavorando con json"
html_title:           "Kotlin: Lavorando con json"
simple_title:         "Lavorando con json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Cos'è e perché
Lavorare con JSON è una pratica molto comune tra i programmatori. JSON (JavaScript Object Notation) è un formato di dati semplice e leggibile per la memorizzazione e lo scambio di informazioni. I programmatori utilizzano JSON perché è facile da usare e supportato da molti linguaggi di programmazione.

## Come fare:
Ecco un esempio di come usare JSON in Kotlin:

```Kotlin
// creazione di un oggetto JSON
val person = """{
    "name": "Marco",
    "age": 30,
    "city": "Milano"
}"""
// parsing dell'oggetto JSON
val json = JSONObject(person)

// per accedere ai dati, è possibile utilizzare il metodo get
val name = json.get("name")
val age = json.get("age")
val city = json.get("city")

// stampa dei risultati
println("Nome: $name")
println("Età: $age")
println("Città: $city")
```
Ecco l'output:
Nome: Marco
Età: 30
Città: Milano

## Approfondimento
JSON è stato sviluppato da Douglas Crockford nel 2001 ed è diventato uno standard per lo scambio di dati tra client e server. Sebbene sia molto popolare, esistono anche altri formati di dati come XML e CSV. Tuttavia, JSON è preferito dai programmatori per la sua semplicità e compatibilità con i browser web.

## Vedi anche
Per ulteriori informazioni su come lavorare con JSON in Kotlin, puoi consultare la documentazione ufficiale di Kotlin: https://kotlinlang.org/docs/reference/server-overview.html#json
Puoi anche esplorare librerie esterne per aiutarti a lavorare con JSON in Kotlin, come ad esempio Gson o Moshi.