---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Fish Shell: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Du denkst vielleicht, warum sollte ich überhaupt ein Datum in der Zukunft oder Vergangenheit berechnen? Nun, es gibt viele Gründe dafür! Man könnte zum Beispiel eine Erinnerung einrichten, um einen wichtigen Termin nicht zu vergessen, oder einen Countdown bis zum nächsten Urlaub berechnen. Die Möglichkeiten sind endlos!

## Wie geht das?

Zum Glück ist es ganz einfach, ein Datum in der Zukunft oder Vergangenheit in der Fish Shell zu berechnen. Alles was du brauchst ist das `date` Kommando und etwas Grundverständnis von Datumsformaten. Hier ist ein Beispiel, um das Datum von heute vor einer Woche zu berechnen:

```Fish Shell
date -v-1w
```

Die Option `-v` ermöglicht es uns, den Wert zu ändern, und `w` steht für eine Woche. Das Ergebnis wird in der Form "Wochentag Monat Tag Zeit Zone Jahr" ausgegeben, zum Beispiel "Thu Aug 5 13:45:00 PDT 2021".

## Tief eintauchen

Wenn du ein besseres Verständnis davon bekommen möchtest, wie genau die Berechnung eines Datums in der Fish Shell funktioniert, dann kannst du einen Blick in die offizielle Dokumentation von `date` werfen. Dort findest du eine detaillierte Beschreibung der verschiedenen Optionen und Formate, die du verwenden kannst.

Eine wichtige Sache zu beachten ist, dass die `date` Syntax zwischen verschiedenen Betriebssystemen variieren kann. Wenn du also auf einem Mac oder Linux-System arbeitest, kannst du möglicherweise mehr Optionen verwenden als auf einem Windows-System.

## Siehe auch

Weitere nützliche Informationen zur Arbeit mit Datumsangaben in der Fish Shell findest du hier:

- [Offizielle `date` Dokumentation](https://fishshell.com/docs/current/cmds/date.html)
- [Codewand: How to Use the date Command in Linux](https://codewand.net/how-to-use-date-in-linux/) (Englisch)
- [Devhints: Fish Shell Guide](https://devhints.io/fish-shell) (Englisch)