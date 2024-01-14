---
title:    "Fish Shell: Eine zukünftige oder vergangene Datum berechnen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Warum

In der Welt des Programmierens gibt es viele nützliche Funktionen, die einem das Leben erleichtern können. Eine solche Funktion ist die Berechnung von Datumsangaben in der Zukunft oder Vergangenheit. Ob für persönliche Erinnerungen oder berufliche Projekte, das Berechnen von Datumsangaben kann sehr hilfreich sein.

# How To

Um Datumsangaben in der Fish Shell zu berechnen, verwenden wir die Funktion `date`. Diese Funktion erwartet drei Parameter: das gewünschte Datum, den gewünschten Offset und das gewünschte Format. Der Offset gibt an, um wie viele Tage das Datum verschoben werden soll. Im folgenden Beispiel berechnen wir das Datum von heute um 5 Tage in der Zukunft und geben es im Format "Tag.Monat.Jahr" aus:

```Fish Shell
date -d "5 days" +"%d.%m.%y"
```
Das Ergebnis lautet dann z.B. "09.07.21". Natürlich ist es auch möglich, Datumsangaben in der Vergangenheit zu berechnen, indem der Offset negativ gesetzt wird. So würde das Datum von heute vor 5 Tagen berechnet werden.

# Deep Dive

Die `date` Funktion ermöglicht es auch, komplexere Datumsangaben zu berechnen, z.B. das Datum von vor 2 Wochen oder das Datum von nächster Woche Montag. Hierfür können verschiedene Zeitbegriffe wie zum Beispiel "1 week ago" oder "next monday" verwendet werden. Auch die Angabe von konkreten Datumswerten wie "12.06.21" ist möglich.

Es ist außerdem möglich, die Berechnung von Datumsangaben in anderen Zeitzonen durchzuführen. Hierfür muss der Parameter `-u` für "UTC" verwendet werden.

# Siehe auch

- Fish Shell Dokumentation über die `date` Funktion: https://fishshell.com/docs/current/cmds/date.html
- Mehr über Zeitberechnungen in der Fish Shell: https://fishshell.com/docs/current/index.html#specifying-time-and-date