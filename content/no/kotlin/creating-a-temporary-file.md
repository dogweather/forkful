---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lage en midlertidig fil er en måte for programmerere å lagre data på kort sikt uten å forstyrre appens langsiktige lagring. Dette er nyttig når du trenger å behandle store datamengder eller bufferdata fra nettverksforespørsler.

## Hvordan:
Å lage en midlertidig fil i Kotlin er enkelt og greit. Her er hvordan du gjør det:

```Kotlin
import java.io.File

fun main() {

    val tempFile: File = createTempFile("midlertidigFil",".txt")
    println("Midlertidig fil opprettet: " + tempFile.absolutePath)
    
}
```
Dette genererer følgende output:
``` 
Midlertidig fil opprettet: /var/folders/hv/rbblq2dx83j27wb3g1yd0dd00000gp/T/midlertidigFil1077988522927703465.txt
```

## Dypdykk:

1. Historisk kontekst: Opprettelse av midlertidige filer har blitt brukt i dataprogrammering siden de tidligste dagene av diskbasert lagring.

2. Alternativer: Du kan lage dine egne midlertidige lagringssystemer, men dette kan innebære mer arbeid og øke sjansen for feil.

3. Implementeringsdetaljer: createTempFile-funksjonen i Kotlin lager en ny tom fil i standard midlertidig filkatalog. Filnavnet blir generert ved å sette sammen prefiks- og suffiksparameterne.

## Se også:
For mer informasjon om Kotlin og I/O-behandlinger, sjekk ut følgende kilder:
- Offisielle Kotlin dokumentasjon: https://kotlinlang.org/docs/tutorials/kotlin-for-py/working-with-files.html
- En grundig guide om I/O operations i Kotlin: https://www.baeldung.com/kotlin-file-io