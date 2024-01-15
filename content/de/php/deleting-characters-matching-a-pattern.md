---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "PHP: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann nützlich sein, wenn man Text oder Daten bereinigen möchte. Zum Beispiel könnte man unerwünschte Zeichen aus einer Benutzereingabe entfernen, bevor diese in einer Datenbank gespeichert werden. Es ist also eine hilfreiche Methode, um Daten sauber und fehlerfrei zu halten.

## Wie funktioniert es?

Um Zeichen, die einem bestimmten Muster entsprechen, aus einer Zeichenkette zu löschen, verwenden wir die Funktion "preg_replace()" in PHP. Diese Funktion erwartet drei Parameter: das Muster, das wir suchen möchten, durch was wir es ersetzen möchten und den Text, in dem wir suchen möchten.

Ein Beispiel dafür, wie wir alle Zahlen aus einem Text entfernen können, könnte wie folgt aussehen:

```PHP
$text = "H3ll0 W0rld!";
$clean_text = preg_replace("/[0-9]/", "", $text);
echo $clean_text;
```
Dieses Skript würde "Hello World!" ausgeben, da alle Zahlen durch nichts ersetzt wurden.

## Tiefergehende Informationen

Die "preg_replace()" Funktion verwendet sogenannte "Reguläre Ausdrücke" (oder RegEx) um nach Mustern zu suchen. RegEx sind spezielle Zeichenfolgen, die verwendet werden, um Muster in Texten zu beschreiben. Zum Beispiel sucht das Muster "/[0-9]/" nach allen Zahlen von "0" bis "9" in einer Zeichenkette.

Es gibt auch viele weitere Möglichkeiten, RegEx zu verwenden, um ein bestimmtes Muster zu finden oder zu ersetzen. Es ist jedoch wichtig zu beachten, dass sie manchmal komplex sein können und Übung erfordern, um sie effektiv zu nutzen.

In PHP gibt es auch andere Funktionen, die zur Bearbeitung von Zeichenketten verwendet werden können, wie zum Beispiel "str_replace()", "substr()" oder "trim()". Es kann hilfreich sein, sich mit diesen vertraut zu machen, um die beste Methode für ein bestimmtes Szenario auszuwählen.

## Siehe auch

- [PHP Manual - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP Manual - Regular Expressions](https://www.php.net/manual/en/regexp.reference.php)
- [W3Schools - PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)