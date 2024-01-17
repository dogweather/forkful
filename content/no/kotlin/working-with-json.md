---
title:                "Å jobbe med json"
html_title:           "Kotlin: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med JSON er essensielt for enhver moderne programmerer. JSON (JavaScript Object Notation) er en populær måte å strukturere og lagre data på. Det brukes ofte til å utveksle data mellom klient og server i webapplikasjoner, men har også blitt brukt i andre programmeringsspråk på grunn av sin enkelhet og allsidighet.

## Slik gjør du det:
Kotlin har innebygde funksjoner for å jobbe med JSON-data. For å lese JSON-data, bruker vi funksjonen ```JSONObject()``` og ```JSONArray()```. Vi kan da bruke kjente metoder som ```get()```, ```put()``` og ```remove()``` for å manipulere dataene. Se et eksempel nedenfor:

```
// Opprett en JSON-string
val jsonString = "{ "name": "Kari", "age": 25, "city": "Oslo" }"

// Konverter til en JSON-objekt
val jsonObject = JSONObject(jsonString)

// Hent ut data fra objektet
val name = jsonObject.get("name") // output: "Kari"
val age = jsonObject.get("age") // output: 25
val city = jsonObject.get("city") // output: "Oslo"
```

For å skrive JSON-data, kan vi bruke funksjonen ```toString()``` sammen med ```put()``` for å legge til data i et JSON-objekt. Se et eksempel nedenfor:

```
// Opprett et tomt JSON-objekt
val jsonObject = JSONObject()

// Legg til data ved hjelp av put()
jsonObject.put("name", "Lars")
jsonObject.put("age", 30)
jsonObject.put("city", "Bergen")

// Konverter til en JSON-string
val jsonString = jsonObject.toString() // output: "{ "name": "Lars", "age": 30, "city": "Bergen" }"
```

## Dypdykk:
JSON ble opprinnelig utviklet for JavaScript, men har blitt populært i mange andre programmeringsspråk på grunn av sin enkelhet og lesbarhet. Et alternativ til JSON er XML, som også brukes til å strukturere data, men er mer kompleks og mindre populær. Implementasjonen av JSON i Kotlin er basert på Java-biblioteket org.json, som gir en enkel og effektiv måte å jobbe med JSON på.

## Se også:
- [Official Kotlin documentation for JSON](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-json-object/)
- [Med Kotlin - En praktisk guide til datakontering med JSON](https://www.ba.no/kotlin/partner/med-kotlin-en-praktisk-guide-til-datakontering-med-json/s/5-8-142855)