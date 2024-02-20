---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:28.459562-07:00
description: "Att tolka HTML inneb\xE4r att dissekera en webbsidas m\xE4rkspr\xE5\
  k till n\xE5got som ett program kan f\xF6rst\xE5 och manipulera. Programmerare tolkar\
  \ HTML f\xF6r att\u2026"
lastmod: 2024-02-19 22:04:57.082926
model: gpt-4-0125-preview
summary: "Att tolka HTML inneb\xE4r att dissekera en webbsidas m\xE4rkspr\xE5k till\
  \ n\xE5got som ett program kan f\xF6rst\xE5 och manipulera. Programmerare tolkar\
  \ HTML f\xF6r att\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML innebär att dissekera en webbsidas märkspråk till något som ett program kan förstå och manipulera. Programmerare tolkar HTML för att extrahera data, automatisera webbinteraktioner eller migrera innehåll mellan system.

## Hur man gör:
Kotlin gör det enkelt att tolka HTML med bibliotek som Jsoup. Så här gör du:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Exempelsida</title></head><body><p>Detta är ett test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Titel: $title")  // Output: Titel: Exempelsida

    val pText = doc.select("p").first()?.text()
    println("Paragraf: $pText")  // Output: Paragraf: Detta är ett test.
}
```

Vi tar titeln och texten i paragrafen, bara för att skrapa på ytan av vad Jsoup kan göra. Men det är en början.

## Fördjupning:
Före Kotlin var Java det självklara valet för detta, ofta på ett klumpigt sätt. Jsoup vände på steken genom att erbjuda ett jQuery-liknande tillvägagångssätt. Att tolka HTML är dock inte exklusivt för Jsoup; andra bibliotek som HtmlUnit eller till och med regex (även om det avråds) finns. Med Jsoup säkerställer du att din tolkning respekterar dokumentets struktur. Det använder en DOM-modell, som möjliggör val och manipulation av element. Det är motståndskraftigt också – det kan tolka även den mest oordnade HTML.

## Se även:
Fördjupa dig i Jsoup:

- Jsoups officiella dokumentation: https://jsoup.org/
- Boken "Kotlin for Android Developers": https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin programmeringsspråkets officiella webbplats: https://kotlinlang.org/

För bredare diskussioner och handledning om webbskrapning och tolkning:

- Webbskrapning med Kotlin och Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Att tolka HTML på Android med Kotlin och Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
