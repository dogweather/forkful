---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.586702-07:00
description: "Das Kapitalisieren eines Strings beinhaltet die Umwandlung des Anfangsbuchstabens\
  \ eines gegebenen Strings in Gro\xDFbuchstaben, w\xE4hrend der Rest in\u2026"
lastmod: '2024-03-13T22:44:53.787591-06:00'
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings beinhaltet die Umwandlung des Anfangsbuchstabens\
  \ eines gegebenen Strings in Gro\xDFbuchstaben, w\xE4hrend der Rest in\u2026"
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Was & Warum?

Das Kapitalisieren eines Strings beinhaltet die Umwandlung des Anfangsbuchstabens eines gegebenen Strings in Großbuchstaben, während der Rest in Kleinbuchstaben gehalten wird. Dies geschieht oft aus Gründen der standardisierten Formatierung oder Lesbarkeit. Programmierer führen diese Aufgabe häufig durch, um sicherzustellen, dass Daten konsistent präsentiert werden, insbesondere in Benutzeroberflächen oder beim Verarbeiten und Anzeigen von Benutzereingaben.

## Wie geht das:

In Elm gibt es keine integrierte Funktion speziell zum Kapitalisieren von Strings. Jedoch können Sie dies leicht erreichen, indem Sie die eingebauten Funktionen des `String`-Moduls wie `toUpper`, `toLower`, `left` und `dropLeft` verwenden.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Beispiel Nutzung
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Ausgabe: "Hello World"
```

Für komplexere Szenarien oder wenn Sie eine Bibliothek bevorzugen, die eine direkte Möglichkeit zum Kapitalisieren von Strings bietet, könnten Sie ein Drittanbieter-Paket wie `elm-community/string-extra` in Betracht ziehen. Wie auch immer, bis zu meinem letzten Update ermutigt das Ökosystem von Elm dazu, solche Aufgaben mit eingebauten Funktionen zu bewältigen, um die Sprache und Projekte schlank zu halten.

```elm
import String.Extra as StringExtra

-- Falls es eine `capitalize` Funktion in einer Drittanbieter-Bibliothek gibt
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Beispiel Nutzung mit hypothetischer Bibliotheksfunktion
main =
    "this is elm" |> capitalizeWithLibrary
    -- Hypothetische Ausgabe: "This is elm"
```

Prüfen Sie immer das Elm-Paketverzeichnis auf die neuesten und bevorzugten Bibliotheken für die String-Manipulation, wenn Sie nach zusätzlicher Funktionalität über die Standardbibliothek hinaus suchen.
