---
title:                "Arbeide med csv"
html_title:           "Kotlin: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kort sagt, arbeider man med CSV når man trenger å lagre data i tabellform for å kunne dele eller bruke senere. Programmere bruker CSV fordi det er en effektiv måte å organisere og behandle store datamengder på.

## Hvordan:
```Kotlin
// For å lese en CSV-fil, bruker man standard library funksjonen readLines() og deler på komma for å få hver verdi som en egen streng.
val fil = File("data.csv")
val innhold = fil.readLines()
innhold.forEach { rad ->
    val verdier = rad.split(',')
}
```

```Kotlin
// For å skrive en CSV-fil, bruker man standard library funksjonen appendText() og kombinerer verdier med komma mellom hver verdi.
val fil = File("data.csv")
fil.appendText("navn, alder, yrke")
var navn = "John"
var alder = 30
var yrke = "utvikler"
fil.appendText("$navn, $alder, $yrke")
```

## Dypdykk:
CSV, eller "Comma-Separated Values", er et enkelt filformat som har vært brukt siden 1970-tallet for å lagre tabell-lignende data. Alternativene til CSV kan være mer komplekse filformater som JSON eller XML, men disse kan være unødvendige for mindre datamengder. Når man arbeider med CSV i Kotlin, er det viktig å håndtere eventuelle komplikasjoner som mellomrom, linjeskift eller sitat-tegn i dataene.

## Se også:
- [https://kotlinlang.org/docs/reference/basic-types.html#string-template](https://kotlinlang.org/docs/reference/basic-types.html#string-template) for informasjon om string templates i Kotlin.
- [https://try.kotlinlang.org/](https://try.kotlinlang.org/) for å prøve ut Kotlin-kode i nettleseren.
- [https://www.json.org/json-en.html](https://www.json.org/json-en.html) for informasjon om JSON-filformatet.