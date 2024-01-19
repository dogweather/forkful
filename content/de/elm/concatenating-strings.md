---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Zusammenführung von Strings (auf Englisch "Concatenation") ist der Vorgang, bei dem zwei oder mehr Strings aneinander gereiht werden. Programmierer nutzen sie, um Daten zu kombinieren oder klar formatierte Ausgaben zu erzeugen.

## So geht's:

In Elm verwenden wir die `++`-Funktion, um Strings zu verbinden. Hier sind ein paar Beispiele:

```Elm
name = "Emma"
nachname = "Schmidt"
vollständigerName = name ++ " " ++ nachname
```
Die Ausgabe von `vollständigerName` wäre dann `Emma Schmidt`.

## Vertiefung:

Die Methode der Zusammenführung von Strings ist fast so alt wie die Programmierung selbst und findet sich in zahlreichen Sprachen wieder.

Alternativen zur direkten String-Verkettung können String-Interpolation oder Format-Funktionen sein, jedoch unterstützt Elm aktuell diese Techniken nicht out-of-box. Sie können jedoch benutzerdefinierte Funktionen erstellen, die ähnliche Ergebnisse liefern.

Was die Implementierung angeht, so ist die `++`-Funktion in Elm sehr effizient und schnell. Beachten Sie jedoch, dass die Verwendung von `++` in einer Schleife eine quadratische Laufzeitkomplexität aufweisen kann, da bei jeder Iteration ein neuer String erstellt wird.

## Weiterführende Links:

- [Elm Programmiersprache Dokumentation](https://elm-lang.org/docs)
- [Elm String Bibliothek](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Funktionen zur Strings-Verarbeitung](https://www.tutorialsteacher.com/elm/elm-string-functions)