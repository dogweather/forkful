---
title:                "Textsuche und -ersetzung"
html_title:           "Gleam: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Was & Warum?

Suchen und Ersetzen von Text ist ein häufiges Werkzeug in der Programmierung, mit dem Programmierer bestimmte Zeichenketten in einem Text finden und ersetzen können. Dies kann nützlich sein, um Fehler zu beheben, Variablen- oder Funktionsnamen anzupassen oder den Code für verschiedene Anforderungen anzupassen.

Wie geht's?

Um in Gleam Text zu suchen und zu ersetzen, können wir die integrierte Funktion `String.replace` verwenden. Hier ist ein Beispiel, wo wir alle Vorkommen von "Hund" in einem Satz durch "Katze" ersetzen:

```Gleam
let ersetzt = String.replace("Ich habe einen Hund", "Hund", "Katze")
```

Das Ergebnis wird "Ich habe eine Katze" sein. Beachte, dass `String.replace` die geänderte Zeichenkette zurückgibt und die ursprüngliche Zeichenkette unverändert lässt. Falls wir mehr als ein Vorkommen ersetzen möchten, können wir den optionale Parameter `limit` verwenden:

```Gleam
let ersetzt = String.replace("Ich habe einen Hund und noch einen Hund", "Hund", "Katze", limit: 1)
```

Das Ergebnis hier wäre "Ich habe eine Katze und noch einen Hund". Der zweite Hund bleibt also unverändert.

Tiefentauchen

Das Suchen und Ersetzen von Text ist seit den frühen Tagen der Programmierung ein wichtiges Werkzeug und wird in vielen Programmiersprachen unterstützt. In manchen Fällen, wie z.B. bei regulären Ausdrücken, ist es jedoch etwas komplexer und erfordert zusätzliche Kenntnisse. Alternativ können auch externe Tools oder Bibliotheken genutzt werden, um bestimmte Such- und Ersatzfunktionen anzubieten.

Siehe auch

Hier sind einige hilfreiche Links, um mehr über die Bearbeitung von Text in Gleam zu lernen:

- Die offizielle Gleam-Dokumentation: https://gleam.run/documentation
- Ein Tutorial zu Strings in Gleam: https://github.com/gleam-lang/gleam/blob/master/docs/tutorials/string.md#replace
- Die offizielle Gleam-Community auf Discord: https://discord.gg/3SvYqYJ