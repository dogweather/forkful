---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ist der Prozess, bei dem HTML-Text in seine Strukturbestandteile zerlegt wird. Programmierer machen das, um die Daten der Webseiten zu analysieren, zu ändern oder zu extrahieren.

## So geht's:
Lasst uns loslegen! Wir werden jsoup verwenden, eine praktische Kotlin-Bibliothek, um HTML zu parsen.

Fügen Sie zuerst die jsoup-Bibliothek zu Ihrem Projekt hinzu.

```Kotlin
dependencies {
  implementation 'org.jsoup:jsoup:1.13.1' 
}
```
Nun können wir eine einfache Webseite analysieren.

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Erste Parsing Seite</title></head>"
              + "<body><p>Hallo Welt!</p></body></html>"
    val doc = Jsoup.parse(html)
    println(doc.title())
    println(doc.body().text())
}
```
Die Ausgabe wird sein:
```
Erste Parsing Seite
Hallo Welt!
```

## Tiefgreifende Besprechung
HTML-Parsing hat eine lange Geschichte, die mit der Entwicklung des Webs selbst zusammenfällt. Ursprünglich erfolgte das HTML-Parsing manuell über String-Manipulation. Mit der Entwicklung von Sprachen wie Python, JavaScript und jetzt Kotlin haben sich Bibliotheken entwickelt, die Parsing einfacher und sicherer machen.

Alternativen zu jsoup umfassen Bibliotheken wie HtmlCleaner und Jericho HTML Parser. Jede dieser Bibliotheken hat Vor- und Nachteile, abhängig von Ihren speziellen Anforderungen.

Beim HTML-Parsing mit jsoup handelt es sich um top-down-Parsing. Erst wird die gesamte HTML-Datei geladen und dann wird der HTML-Baum von oben nach unten analysiert. Dies ist wichtig zu wissen, wenn Sie mit sehr großen HTML-Dokumenten arbeiten, da dies ihren Speicherbedarf beeinflusst.

## Siehe auch
Für weitere Informationen über das HTML-Parsing in Kotlin, werfen Sie einen Blick auf folgende Quellen:
1. [jsoup Dokumentation](https://jsoup.org/cookbook/)
4. [Kotlin-Dokumentation](https://kotlinlang.org/docs/reference/)