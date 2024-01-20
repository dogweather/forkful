---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:32:27.900357-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)

Il parsing di HTML significa estrarre dati da una pagina web. I programmatori lo fanno per automatizzare raccolta di informazioni, fare scraping di contenuti o manipolare e interagire con siti web.

## How to: (Come fare:)

Utilizziamo la libreria Jsoup per fare parsing di HTML con Kotlin. Jsoup fornisce un DOM, simile a quello di JavaScript, per navigare e manipolare il codice HTML.

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = """
        <html>
            <head>
                <title>Saluti da Kotlin!</title>
            </head>
            <body>
                <p id="message">Ciao Mondo!</p>
            </body>
        </html>
    """

    val doc = Jsoup.parse(html)
    val message = doc.select("#message").first()?.text()

    println(message) // Stampa: Ciao Mondo!
}
```

## Deep Dive (Approfondimento)

Parsing HTML con Kotlin non è compleso grazie a libreria come Jsoup. Nata nel 2009, Jsoup ha reso il lavoro con HTML un gioco da ragazzi. Alternativamente, puoi usare regex o XML parser, ma non sono altrettanto maneggevoli o sicuri per questo compito. Jsoup gestisce HTML reale e imperfetto, correggendo anche documenti malformati.

## See Also (Vedi anche)

- Jsoup Official Documentation: [Jsoup Documentation](https://jsoup.org/)
- Kotlin Official Site: [Kotlin](https://kotlinlang.org/)