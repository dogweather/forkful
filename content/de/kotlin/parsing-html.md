---
title:                "Kotlin: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

# Warum

HTML ist die Grundlage für viele Webseiten und Anwendungen. Durch das Parsen von HTML können wir die Informationen dieser Seiten extrahieren und sie für unsere Zwecke nutzen. Dies kann beispielsweise für das Scraping von Daten oder für die Erstellung von automatisierten Scripts hilfreich sein.

# Wie man HTML mit Kotlin parsen kann

Das Parsen von HTML mit Kotlin ist relativ einfach. Zunächst benötigen wir die Bibliothek Jsoup, die uns dabei helfen wird, das HTML zu analysieren. Wir binden diese Bibliothek in unserem Kotlin-Projekt ein, indem wir sie zuerst als Abhängigkeit in unserer build.gradle Datei hinzufügen:

```Kotlin
dependencies {
    implementation 'org.jsoup:jsoup:1.14.3'
}
```

Als nächstes importieren wir die Klasse Jsoup in unserem Code:

```Kotlin
import org.jsoup.Jsoup
```

Nun können wir eine Verbindung zu einer Webseite herstellen und das HTML parsen. Hier ist ein Beispiel, wie wir den Titel einer Webseite extrahieren können:

```Kotlin
val document = Jsoup.connect("https://www.beispielwebseite.de").get()
val pageTitle = document.title()
println(pageTitle)
```

Die Ausgabe wird der Titel der Webseite sein, in diesem Fall "Beispielwebseite".

# Tiefergehende Information zum Parsen von HTML

Jsoup bietet viele nützliche Methoden zum Extrahieren von Informationen aus HTML. So können wir beispielsweise mit der `select()` Methode bestimmte Elemente auf einer Seite auswählen und deren Inhalt auslesen. Diese Methode verwendet CSS-ähnliche Selektoren, um Elemente zu finden.

```Kotlin
val headlines = document.select("h1")
for (headline in headlines) {
    println(headline.text())
}
```

Dieser Code würde alle Überschriften auf der Webseite auslesen und ausgeben.

Es gibt noch weitere Funktionen und Methoden, die uns das Parsen von HTML einfacher machen. Es lohnt sich also, sich tiefer in die Dokumentation von Jsoup einzulesen, um alle Möglichkeiten auszuschöpfen.

# Siehe auch

- [Offizielle Jsoup Dokumentation](https://jsoup.org/cookbook/)
- [Kotlin Webseite](https://kotlinlang.org/docs/)