---
title:                "Fish Shell: Die Länge eines Strings finden"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings scheint vielleicht eine einfache Aufgabe zu sein, aber in der Welt des coding kann es einen großen Unterschied machen. Es kann hilfreich sein, eine stringbasierte Funktion zu schreiben oder einfach nur die Länge eines Benutzernamens zu überprüfen. Egal aus welchem ​​Grund, das Finden der Länge eines Strings ist eine wesentliche Fähigkeit für jeden, der mit Fish Shell programmiert.

## Wie geht das?

Eine der einfachsten Möglichkeiten, um die Länge eines Strings in Fish Shell zu finden, ist die Verwendung des integrierten `string length` Befehls. Dieser Befehl gibt die Anzahl der Zeichen in einem String zurück, einschließlich Leerzeichen. Hier ist ein Beispiel in Fish Shell:

```Fish Shell
set user "Max Mustermann"
echo (string length $user)
```
Dieser Code würde die Ausgabe `15` erzeugen, da der String "Max Mustermann" 15 Zeichen lang ist. Es ist wichtig zu beachten, dass die Länge eines Strings immer ein ganzzahliges Ergebnis ist, unabhängig von der Anzahl der Leerzeichen.

## Tiefer gehende Informationen

Wenn Sie sich intensiver mit der Länge von Strings in Fish Shell befassen möchten, gibt es ein paar Dinge zu beachten. Zum einen ist es wichtig zu wissen, dass der `string length` Befehl auch Leerzeichen und andere Sonderzeichen mitzählt, die möglicherweise im String vorhanden sind. Dies kann die tatsächliche Länge des nutzbaren Textes im String beeinflussen.

Eine weitere hilfreiche Funktion ist `string upper`, die einen String in Großbuchstaben umwandelt, und `string lower`, die den String in Kleinbuchstaben umwandelt. Diese Funktionen können beim Vergleichen von Strings hilfreich sein, da sie Groß- und Kleinschreibung ignorieren können.

## Siehe auch

Weitere Informationen über die stringbasierte Programmierung in Fish Shell finden Sie in der offiziellen Dokumentation oder in folgenden Links:

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Einführung in die Fish Shell Programmierung](https://dzone.com/articles/getting-started-fish-shell)
- [10 nützliche Tricks für Fish Shell](https://blog.balthazar-rouberol.com/10-useful-fish-shell-tricks-you-should-know)