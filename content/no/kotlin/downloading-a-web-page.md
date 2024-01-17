---
title:                "Nedlasting av en nettside"
html_title:           "Kotlin: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Nedlasting av en nettside er rett og slett å hente innholdet fra en nettside og lagre det på din egen enhet. Programmere gjør dette for å kunne bruke innholdet på ulike måter, som for eksempel å analysere informasjon, manipulere data eller for å inkludere nettsiden i en annen applikasjon.

# Hvordan gjør man det?
```Kotlin 
import java.net.URL
import java.io.File

fun main() {
    // Opprett et URL objekt med linken til nettsiden du ønsker å laste ned
    val url = URL("https://www.example.com")

    // Les innholdet fra URL og lagre det i en fil
    File("nettside.html").writeText(url.readText())

    // Skriv ut en beskjed om nedlastingen var en suksess
    println("Nettsiden ble lastet ned og lagret som 'nettside.html'")
}
```

# Dykk dypere
For å forstå hvorfor nedlasting av nettsider er viktig, må vi vite at internettet er bygget opp av ulike protokoller. En av disse er HTTP, som brukes til å hente og sende informasjon mellom klienter og servere. En annen måte å laste ned nettsider på er ved å bruke en nettleser, men dette kan være vanskelig hvis man ønsker å behandle informasjonen videre. I Kotlin kan man også bruke tredjepartsbiblioteker som OkHttp for mer avansert nedlastning av nettsider.

# Se også
- [HTTP protokollen](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [OkHttp biblioteket](https://square.github.io/okhttp/)