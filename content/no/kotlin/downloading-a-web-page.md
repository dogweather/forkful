---
title:                "Kotlin: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Enkelt og greit, noen ganger trenger vi å laste ned en nettside for å få tak i informasjon eller data. Å laste ned en nettside kan være nyttig for utviklere som ønsker å analysere eller manipulere data, eller for forskere og journalister som ønsker å undersøke informasjon for forskning eller nyhetsartikler.

## Hvordan 
Å laste ned en nettside er en relativt enkel prosess med Kotlin. Følgende kode eksempel viser hvordan man kan laste ned en nettside og få tak i kildekoden:

```Kotlin
val url = URL("https://www.example.com")
val conn = url.openConnection() as HttpURLConnection
conn.requestMethod = "GET"
val responseCode = conn.responseCode

if(responseCode == HttpURLConnection.HTTP_OK) { //sjekk om responsen er OK
    // Bruk "inputStream" til å lese kildekoden fra nettsiden
    val inputStream = conn.inputStream
    val reader = BufferedReader(InputStreamReader(inputStream))
    var line: String?
    val response = StringBuffer()

    // Les hver linje av kildekoden og lagre det i en StringBuffer
    do {
        line = reader.readLine()
        response.append(line)
    } while (line != null)

    // Skriv ut kildekoden
    println(response.toString())
}

// Lukk tilkoblingen
conn.disconnect()
```

Kjører man denne koden vil man få den komplette kildekoden til nettsiden som output i konsollen.

## Dypdykk
Det finnes også biblioteker og rammeverk i Kotlin som gjør det enda enklere å laste ned en nettside og håndtere responsen. Et eksempel er `OkHttp`, som er et populært HTTP- og HTTP/2 klient bibliotek som kan brukes til å hente data fra en nettside med bare noen få linjer med kode.

I tillegg finnes det også flere tredjeparts biblioteker og verktøy som kan hjelpe med å analysere og håndtere data som er lastet ned fra en nettside, som for eksempel `Jsoup` som er et Java HTML parser bibliotek som også støtter Kotlin.

Å laste ned en nettside kan være viktig for å få tak i data som kan brukes til å utvikle nye applikasjoner, eller til forskning og journalistikk. Med Kotlin er det enkelt å implementere en løsning for å laste ned og behandle data fra en nettside.

## Se også
- [OkHttp - HTTP and HTTP/2 client for Kotlin](https://github.com/square/okhttp)
- [Jsoup - Java HTML parser for Kotlin](https://jsoup.org/)