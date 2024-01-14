---
title:    "Elm: Verwendung von regulären Ausdrücken"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug, das in vielen Programmiersprachen verwendet wird. Sie ermöglichen es uns, komplexe Muster in Strings zu identifizieren und zu verarbeiten. In Elm können sie besonders nützlich sein, um Text zu analysieren oder validieren. Mit nur wenigen Zeichen können wir effizient eine große Anzahl von Strings durchsuchen und bearbeiten.

# Wie

Um reguläre Ausdrücke in Elm zu verwenden, müssen wir zunächst das `Regex`-Modul importieren. Dann können wir mit der Funktion `Regex.regex` unseren regulären Ausdruck definieren. Dabei müssen wir einige wichtige Dinge beachten:

1. Wir müssen den regulären Ausdruck als String angeben und ihn mit `\` escapen, um Sonderzeichen zu erkennen.
2. Wenn wir Zeichen wie `"` oder `\` im regulären Ausdruck verwenden möchten, müssen wir auch diese escapen.
3. Wir können mit `Regex.find` den regulären Ausdruck auf einem String anwenden und erhalten als Ergebnis eine Liste von Matches.
4. Mit `Regex.replace` können wir einen regulären Ausdruck auf einem String anwenden und bestimmte Teile ersetzen.

Beispiel:

```elm
import Regex exposing (regex, find, replace)

-- Definiere den regulären Ausdruck
regexPattern : Regex.Pattern
regexPattern = regex "(\\d{2})-(\\d{2})-(\\d{4})" -- Sucht nach dem Muster TT-MM-JJJJ

-- Wende den regulären Ausdruck auf einen String an
string : String
string = "Heute ist der 01-10-2021"

matches : List ( List String )
matches = find regexPattern string -- [["01", "10", "2021"]]

replacement : String
replacement = replace regexPattern (\_ -> "XX-XX-XXXX") string -- "Heute ist der XX-XX-XXXX"

```

# Tiefgehende Einblicke

Es gibt eine Vielzahl von Funktionen im `Regex`-Modul, die es uns ermöglichen, noch komplexere und leistungsstärkere Ausdrücke zu definieren und anzuwenden. Hier sind einige davon:

- `Regex.replace` mit einer Funktion als zweiten Parameter, um dynamische Ersetzungen durchzuführen.
- `Regex.contains` und `Regex.all` zum Überprüfen, ob ein String einem bestimmten Muster entspricht oder alle Vorkommen eines Musters zu finden.
- `Regex.captures` zum Extrahieren von bestimmten Teilen eines komplexen Musters.
- `Regex.range` zum Festlegen der Positionen von Matches und Submatches.

Mit diesen Funktionen können wir anpassungsfähigere und präzisere reguläre Ausdrücke erstellen und so noch komplexere Textverarbeitungsaufgaben in Elm lösen.

# Siehe auch

- Offizielle Dokumentation des `Regex`-Moduls: https://package.elm-lang.org/packages/elm/regex/latest/
- Ein interaktives Tutorial zu regulären Ausdrücken in Elm: https://jfairbank.github.io/regex-elm-docs/
- Ein Artikel über die effiziente Verwendung von regulären Ausdrücken in Elm: https://www.samharris.dev/posts/high-performance-elm-regex/