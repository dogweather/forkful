---
title:    "C: Fehlerausgabe drucken"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wichtiges Werkzeug beim Programmieren in C. Sie helfen dabei, Fehler zu identifizieren und zu beheben, indem sie Informationen über den aktuellen Zustand des Programms liefern. Das kann insbesondere bei komplexen Programmen sehr hilfreich sein.

## Wie man Debug-Ausgaben macht

Um Debug-Ausgaben in C zu erstellen, können wir die Funktion `printf()` verwenden. Diese Funktion erlaubt es uns, eine Nachricht auszugeben, die wir im Programm ausgewählt haben. Die Syntax ist ähnlich wie bei `printf()` in anderen Programmiersprachen, aber es gibt einige wichtige Unterschiede.

```C
printf("Debug-Ausgabe: %s\n", "Hier ist meine Nachricht");
```

Das erste Argument in `printf()` ist ein sogenanntes "Formatierungs-String", der bestimmt, wie die Ausgabe formatiert werden soll. In unserem Beispiel oben verwenden wir `%s`, um einen String-Wert anzuzeigen. Anstelle von `%s` können wir auch andere Formatierungsoptionen wie `%d` für Ganzzahlen oder `%f` für Fließkommazahlen verwenden. Das zweite Argument in `printf()` ist der Wert, der anstelle des Formatierungs-Strings angezeigt werden soll.

## Tiefere Einblicke

Es gibt auch Möglichkeiten, `printf()` für Debug-Ausgaben noch effizienter zu nutzen. Wir können zum Beispiel den Präprozessor-Befehl `#define` verwenden, um einen Makro-Namen für unsere Debug-Ausgabe zu definieren. Dies erleichtert das Schreiben von Debug-Ausgaben, da wir anstelle des langen `printf()`-Aufrufs einfach das definierte Makro verwenden können.

```C
#define DEBUG(msg) printf("Debug-Ausgabe: %s\n", msg);

// ...
int zahl = 7;
DEBUG("Die Zahl ist: %d", zahl);
```

Eine andere nützliche Technik ist das Hinzufügen von Bedingungen zu unseren Debug-Ausgaben. Wir können `if`-Anweisungen verwenden, um zu überprüfen, ob bestimmte Variablen oder Zustände erfüllt sind, bevor wir eine Debug-Ausgabe ausführen. Dadurch können wir wählen, welche Debug-Ausgaben wir im Programm sehen möchten, abhängig von bestimmten Bedingungen.

## Siehe auch

- [C Debugging Tutorial](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [Using printf() for debugging in C](https://www.geeksforgeeks.org/using-printf-debugging-c/)
- [Debugging C Programs with printf](https://www.codesdope.com/blog/article/debugging-c-programs-with-printf/)