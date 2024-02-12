---
title:                "Nedlasting av en nettside"
aliases:
- /no/kotlin/downloading-a-web-page/
date:                  2024-01-20T17:44:24.753677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Nedlasting av nettsider er henting av data fra en URL for å se eller behandle informasjonen. Programmerere gjør dette for å trekke ut data, automatisere oppgaver eller integrere tjenester.

## How to:
Kotlin gjør det enkelt. Her er en kjapp kode som viser hvordan du laster ned innholdet på en nettside:

```Kotlin
import java.net.HttpURLConnection
import java.net.URL

fun downloadWebpage(urlToDownload: String): String {
    val url = URL(urlToDownload)
    val httpConnection = url.openConnection() as HttpURLConnection
    return httpConnection.inputStream.bufferedReader().readText()
}

fun main() {
    val webpageContent = downloadWebpage("https://example.com")
    println(webpageContent)
}
```

Kjører du dette, skal output lignende dette vises:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive:
Nedlasting av nettsider går langt tilbake til nettets tidlige dager, da det ofte ble gjort via terminaler og enkle script. Alternativer i Kotlin inkluderer biblioteker som `khttp` eller `Fuel` for mer komplekse behov.

Når det gjelder implementeringsdetaljer, åpner koden over en HTTP-tilkobling og leser innholdet som en streng. Dette er funksjonelt for enkel bruk, men moderne applikasjoner bruker ofte tredjepartsbiblioteker for å håndtere cookies, omadresseringer og asynkronitet på en mer robust måte.

## See Also:
- Kotlin docs for HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-url-connection/
- khttp GitHub repo: https://github.com/jkcclemens/khttp
- Fuel GitHub repo: https://github.com/kittinunf/fuel
