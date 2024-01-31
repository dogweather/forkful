---
title:                "HTML parsen"
date:                  2024-01-20T15:32:37.587235-07:00
simple_title:         "HTML parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parser lesen und interpretieren den Inhalt von Webseiten, damit Apps und Services die Daten nutzen können. Wir programmieren sowas, um Inhalte automatisch zu verarbeiten, wie z.B. Scraping oder Datenextraktion.

## Anleitung
```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Beispielseite</title></head>" +
               "<body><p>Dies ist ein Beispiel</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    val bodyText = doc.body().text()

    println("Titel der Seite: $title")
    println("Inhalt des Body: $bodyText")
}

// Ausgabe:
// Titel der Seite: Beispielseite
// Inhalt des Body: Dies ist ein Beispiel
```
Jsoup ist eine Java-Bibliothek, die auch in Kotlin verwendet werden kann. Sie analysiert den HTML-Code und erlaubt einfachen Zugriff auf die Elemente.

## Tiefgang
Historisch gesehen wurden für das Parsen von HTML oft reguläre Ausdrücke verwendet, was aber zu fehleranfälligen Lösungen führte. Moderne Bibliotheken wie Jsoup verwenden einen DOM-Parser, der wesentlich robuster und sicherer ist. Alternativen zu Jsoup sind z.B. HtmlCleaner oder jsoup: Java HTML Parser. Beim Parsen sollte man auf Performance und korrekte Fehlerbehandlung achten, gerade bei komplexen oder fehlerhaften HTML-Dokumenten.

## Siehe auch
- Jsoup: [Offizielle Webseite](https://jsoup.org/)
- Jsoup GitHub Repository: [https://github.com/jhy/jsoup](https://github.com/jhy/jsoup)
- W3C HTML5 Spezifikation: [https://www.w3.org/TR/html5/](https://www.w3.org/TR/html5/)
- Kotlin Programmierung: [https://kotlinlang.org/](https://kotlinlang.org/)
