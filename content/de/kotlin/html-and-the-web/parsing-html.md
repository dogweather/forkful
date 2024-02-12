---
title:                "HTML parsen"
date:                  2024-02-03T19:12:18.603364-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
HTML zu parsen bedeutet, das Markup einer Webseite so zu zergliedern, dass es von einem Programm verstanden und manipuliert werden kann. Programmierer parsen HTML, um Daten zu extrahieren, Web-Interaktionen zu automatisieren oder Inhalte zwischen Systemen zu migrieren.

## Wie geht das:
Kotlin macht das Parsen von HTML mit Bibliotheken wie Jsoup unkompliziert. So geht's:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Beispielseite</title></head><body><p>Dies ist ein Test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Titel: $title")  // Ausgabe: Titel: Beispielseite

    val pText = doc.select("p").first()?.text()
    println("Absatz: $pText")  // Ausgabe: Absatz: Dies ist ein Test.
}
```

Wir greifen den Titel und den Text des Absatzes ab, um nur an der Oberfläche zu kratzen, was Jsoup alles kann. Aber es ist ein Anfang.

## Tiefer gehend:
Vor Kotlin war Java der Standard dafür, oft umständlich. Jsoup hat das Blatt gewendet, indem es einen jQuery-ähnlichen Ansatz bot. Das Parsen von HTML ist jedoch nicht exklusiv für Jsoup; andere Bibliotheken wie HtmlUnit oder sogar Regex (obwohl davon abgeraten wird) existieren auch. Mit Jsoup stellen Sie sicher, dass Ihr Parsen die Struktur des Dokuments respektiert. Es verwendet ein DOM-Modell, das die Auswahl und Manipulation von Elementen ermöglicht. Es ist auch widerstandsfähig – es kann sogar das unordentlichste HTML parsen.

## Siehe auch:
Tauchen Sie tiefer in Jsoup ein:

- Jsoup offizielle Dokumentation: https://jsoup.org/
- Buch "Kotlin for Android Developers": https://antonioleiva.com/kotlin-android-developers-book/
- Offizielle Website der Kotlin-Programmiersprache: https://kotlinlang.org/

Für breitere Diskussionen und Anleitungen zum Web Scraping und Parsen:

- Web Scraping mit Kotlin und Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- HTML-Parsing auf Android mit Kotlin und Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
