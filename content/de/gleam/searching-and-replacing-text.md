---
title:                "Suchen und Ersetzen von Text"
html_title:           "Gleam: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Ein einfaches Suchen und Ersetzen von Text kann mühsam und zeitaufwendig sein. Mit der aktuellen Version von Gleam kannst du dieses Problem jedoch mit nur wenigen Codezeilen lösen und dir viel Zeit und Arbeit ersparen.

## So geht's

Um Text in Gleam zu suchen und zu ersetzen, musst du zuerst die Standardbibliothek `gleam/text` importieren. Hier ist ein Beispiel, wie du den Text "Hallo Welt" in "Guten Tag" umwandelst:

```
import gleam/text
let input = "Hallo Welt"
let pattern = "Hallo"
let replacement = "Guten Tag"
let result = replace(input, pattern, replacement)

```

Das Ergebnis ist nun `"Guten Tag Welt"`. Einfach, oder? Du kannst auch reguläre Ausdrücke verwenden, um noch flexiblere Suchmöglichkeiten zu haben.

```
import gleam/text
let input = "Die Sonne scheint"
let pattern = "[^ ]+"
let replacement = "die"
let result = replace(input, pattern, replacement)

```

Das Ergebnis ist nun `"die die die"`. Mit regulären Ausdrücken kannst du auch Groß- und Kleinschreibung ignorieren oder spezielle Zeichen hinzufügen.

## Tiefgehende Infos

Du kannst auch tiefer in das Thema suchen und ersetzen eintauchen, indem du dir die Dokumentation von `gleam/text` ansiehst. Dort findest du alle verfügbaren Funktionen und ihre Verwendung. Außerdem kannst du die Musterlösungen der Community durchsuchen und deine eigenen Fähigkeiten im Umgang mit Texttransformationen verbessern.

## Siehe auch

- Die offizielle Dokumentation von `gleam/text`: https://gleam.run/libraries/text
- Reguläre Ausdrücke erklärt: https://www.regular-expressions.info/
- Die Gleam-Community auf Discord: https://discord.gg/5Ju6gHzm