---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:32:32.480293-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML pozwala nam na wydobywanie i manipulację danymi ze stron internetowych. Programiści robią to, aby pobierać potrzebne informacje lub interaktywnie integrować się z innymi serwisami webowymi.

## How to:
Parsowanie HTML w Kotlinie jest proste z biblioteką jsoup. Oto jak:

```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Przykładowy Tytuł</title></head>" +
               "<body><p>Witaj, Kotlin!</p></body></html>"
    val doc = Jsoup.parse(html)
    
    // Wydobądź treść tytułu
    val title = doc.title()
    println("Tytuł: $title")

    // Znajdź paragraf i wydrukuj jego zawartość
    val pText = doc.select("p").first().text()
    println("Paragraf: $pText")
}
```
Wyjście próbkowe:
```
Tytuł: Przykładowy Tytuł
Paragraf: Witaj, Kotlin!
```

## Deep Dive:
Parsowanie HTML zyskało na znaczeniu wraz z boomem na WWW w latach 90'. Do dziś, to podstawowa umiejętność w web scrapingu i data miningu.

Alternatywami dla jsoup mogą być biblioteki jak HtmlUnit czy AngleSharp (dla C#) oraz mechanizmy wbudowane w język (np. lxml w Pythonie).

Implementacja w Kotlinie z jsoup jest wygodna, ponieważ biblioteka ta jest wystarczająco elastyczna, by obsłużyć różnorodne struktury HTML, a także zapewnia potężne API do selekcji i manipulacji elementami.

## See Also:
- Jsoup Documentation: https://jsoup.org/
- Wprowadzenie do Web Scrapingu z Kotlinem: https://medium.com/@ssaurel/learn-how-to-make-web-scraping-in-kotlin-542043c43b96
- HTMLUnit: http://htmlunit.sourceforge.net/
- AngleSharp (biblioteka .NET, ale dobra dla inspiracji): https://anglesharp.github.io/
