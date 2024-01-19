---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran (request) innebär att begära data från en server. Programmerare gör detta för att samla in, läsa, uppdatera, eller radera data på servern i realtid.

## Hur gör man:

För att skicka en HTTP-begäran i Kotlin, kan vi använda biblioteket `khttp`. Låt oss ta en POST-begäran som exempel:

```Kotlin
import khttp.responses.Response
import khttp.post

fun main() {
    val response : Response = post(
        "https:\/\/eksempel.com/api",
        data = mapOf("namn" to "Kotlin")
    )
    println(response.statusCode)
    println(response.text)
}
```

Resultatet kan se ut så här:

```Output
200
{"meddelande":"Request lyckades"}
```

## Djupdykning:

1. **Historia**: HTTP-begäran är själva ryggraden i webben och har varit det sedan dess uppkomst 1990. HTTP står för HyperText Transfer Protocol.

2. **Alternativ**: Det finns flera alternativ för att skicka HTTP-begäran i Kotlin, som `OkHttp`, `Retrofit` och `Fuel`.

3. **Genomförande detaljer**: När vi skickar en POST-begäran, sänds datan som en part i kroppen av begäran. Serverns svar skickas tillbaka som svarspayload. I vårt Kotlin-exempel ovan kastar `Response` ett undantag om begäran misslyckades. Det returnerar också information om statuskoden, rubriker, och kroppen i svaret.

## Se även:

Nedan är några användbara resurser för vidare läsning:

- Förstå HTTP-begäran: https://sv.wikipedia.org/wiki/Hypertext_Transfer_Protocol
- `khttp` dokumentation: https://jitpack.io/p/jkcclemens/khttp
- `OkHttp` dokumentation: https://square.github.io/okhttp/
- `Retrofit` dokumentation: https://square.github.io/retrofit/
- `Fuel` dokumentation: https://github.com/kittinunf/fuel