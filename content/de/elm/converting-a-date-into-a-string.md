---
title:                "Elm: Ein Datum in eine Zeichenfolge umwandeln"
simple_title:         "Ein Datum in eine Zeichenfolge umwandeln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Situationen in der Entwicklung von Webanwendungen, in denen es notwendig ist, ein Datum in einem bestimmten Format anzuzeigen. Die Verwendung von Strings ist eine gängige Methode, um dies zu erreichen, und in diesem Blog-Beitrag werden wir uns ansehen, wie man in Elm ein Datum in einen String umwandeln kann.

## Wie geht das?

Um ein Datum in Elm in einen String umzuwandeln, müssen wir die Elm-Paketbibliothek "Time" verwenden. Zunächst müssen wir das Modul "Time" importieren, indem wir folgende Zeile in unserem Code hinzufügen:

```Elm
import Time
```

Als nächstes müssen wir eine Funktion namens "format" verwenden, die uns eine benutzerdefinierte Formatierung für unser Datum ermöglicht. Hier ist ein Beispiel, in dem wir ein Datum in ein Datum im Format "D // M // J" umwandeln:

```Elm
dateAsString = Time.format "%d // %m // %Y" date
```

In diesem Fall ist "date" ein Datum im Elm-Format. Das Ergebnis der Konvertierung wird in der Variable "dateAsString" gespeichert.

## Tiefgang

Um zu verstehen, wie die "format" Funktion genau funktioniert, müssen wir uns die Datumsformatierung genauer ansehen. Das Modul "Time" bietet uns eine Reihe von Symbolen, die wir in unserer benutzerdefinierten Formatierung verwenden können. Hier sind einige gängige Symbole, die beim Konvertieren von Datumsformaten verwendet werden:

- %Y: Vierstellige Jahreszahl
- %m: Zweistellige Monatszahl
- %d: Zweistelliger Tag im Monat
- %H: Stunde im 24-Stunden-Format
- %M: Minute
- %S: Sekunde

Es gibt noch viele weitere Symbole, die Sie in der offiziellen Dokumentation des "Time" -Moduls nachschlagen können.

## Siehe auch

- Python strftime reference: https://strftime.org/
- Elm Time documentation: https://package.elm-lang.org/packages/elm/time/latest/Time