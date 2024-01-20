---
title:                "Einen String großschreiben"
html_title:           "Elm: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Zeichenketten groß zu schreiben bedeutet, den ersten Buchstaben eines Wortes oder jeder Wort in einer Zeichenkette in Großbuchstaben umzuwandeln. Programmierer tun dies, um die Lesbarkeit zu verbessern oder Textdaten zu normalisieren.

## So macht man's:

In Elm können wir die `String.toUpper` Funktion verwenden, um eine Zeichenkette zu kapitalisieren. Hier ist ein einfaches Beispiel:

```elm
import String

capitalizedString : String -> String
capitalizedString str =
    String.toUpper str

main =
    print (capitalizedString "hallo welt")
```

Ausgabe:

```
HALLO WELT
```

Bitte beachten Sie, dass dies alle Buchstaben in der Zeichenkette in Großbuchstaben umwandelt.

## Tiefgehende Informationen:

1. Historischer Hintergrund: Die Funktion `toUpper` ist seit den frühesten Versionen von Elm verfügbar und ist ein wesentliches Werkzeug für Textmanipulation und Normalisierung.

2. Alternativen: Es gibt Pakete von Drittanbietern, die zusätzliche Funktionen zur Textmanipulation bieten, einschließlich fortschrittlicherer Kapitalisierungsoptionen.

3. Implementierungsdetails: Intern verwendet Elm unicode-fähige Funktionen zur Durchführung dieser Operation, um sicherzustellen, dass sie mit einer breiten Palette von Schriftsystemen funktioniert.

## Siehe auch:

- Elm String API-Dokumentation: [Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)