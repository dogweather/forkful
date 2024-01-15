---
title:                "Arbeid med json"
html_title:           "Kotlin: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med JSON kan være en viktig del av å utvikle programvare og nettsider. JSON er en enkel og effektiv måte å lagre og utveksle data på, og blir derfor mye brukt i moderne utvikling.

## Slik gjør du det

```Kotlin
// Henter JSON fra en URL og lagrer det som en String
val jsonObject = URL("https://api.example.com/data").readText()

// Konverterer Stringen til et JSON-objekt
val data = JSONObject(jsonObject)

// Henter spesifikk informasjon fra JSON-objektet
val name = data.getString("name")
val age = data.getInt("age")
val hobbies = data.getJSONArray("hobbies")

// Skriver ut informasjonen i konsollen
println("Navn: $name")
println("Alder: $age")
println("Hobbyer:")
for(i in 0 until hobbies.length()) {
    val hobby = hobbies.getString(i)
    println("- $hobby")
}
```

Output:

```text
Navn: Jane Doe
Alder: 25
Hobbyer:
- Coding
- Hiking
- Reading
```

## For en dypere forståelse

JSON er basert på nøkkel-verdi par, der nøklene er navn på data og verdiene er selve dataen. Dette gjør det enkelt å lagre og hente ut spesifikk informasjon. JSON-objekter kan også inneholde andre JSON-objekter og arrays, noe som gjør det fleksibelt og godt egnet for kompleks datastrukturering.

Det er viktig å merke seg at JSON-objekter må følge et JSON-syntaks, ellers vil det resultere i en feil. Det er derfor viktig å validere JSON før du bruker det i koden din.

## Se også

- [Hva er JSON?](https://www.json.org/json-en.html)
- [Kotlin dokumentasjon](https://kotlinlang.org/docs/home.html)
- [JSON Parser for Kotlin](https://github.com/MicroUtils/kotlin-logging/blob/master/doc/json.md)