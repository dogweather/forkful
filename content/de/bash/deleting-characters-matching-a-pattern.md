---
title:    "Bash: Musterübereinstimmende Zeichen löschen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Bash-Programmierung sehr nützlich sein. Damit können unerwünschte Zeichen aus einer Datei oder einem Text entfernt werden, bevor sie weiterverarbeitet werden.

## Wie man Zeichen mit einem Muster löscht

Um Zeichen mit einem Muster zu löschen, verwenden wir das ```sed```-Kommando in der Bash. Hier ist ein einfaches Beispiel, um alle Zahlen aus einem String zu löschen:

```Bash
echo "123abc456" | sed 's/[0-9]//g'
```

In diesem Beispiel wird das ```sed```-Kommando verwendet, um alle numerischen Zeichen im String zu finden und zu löschen. Das Ergebnis ist "abc".

## Tiefergehende Informationen

Das Löschen von Zeichen mit einem Muster kann auch mit regulären Ausdrücken erfolgen. Diese ermöglichen es uns, noch spezifischere Muster anzugeben, die gelöscht werden sollen. Zum Beispiel könnten wir mit dem folgenden Befehl alle Leerzeichen aus einem String entfernen:

```Bash
echo "Dies ist ein Beispiel" | sed 's/[[:blank:]]//g'
```

Hier verwenden wir den regulären Ausdruck ```[[:blank:]]```, der alle Leerzeichen und Tabulatoren erfasst. Das Ergebnis ist "DiesisteinBeispiel".

Es ist auch möglich, nur bestimmte Zeichen zu löschen, indem wir diese in eckige Klammern innerhalb des regulären Ausdrucks angeben. Zum Beispiel löscht der folgende Befehl alle Vokale aus einem String:

```Bash
echo "Hallo Welt" | sed 's/[aeiou]//g'
```

Das Ergebnis ist "Hll Wlt".

Diese tiefergehenden Techniken bieten uns eine große Flexibilität bei der Bearbeitung von Texten und Dateien in der Bash.

## Siehe auch

- [Ein ausführlicherer Leitfaden zur Verwendung von sed](https://www.digitalocean.com/community/tutorials/how-to-use-sed-to-find-and-replace-text-in-files-in-linux)
- [Weitere Informationen zu regulären Ausdrücken in der Bash](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)

Mit dem Wissen über das Löschen von Zeichen mit einem Muster in der Bash kannst du deine Skripte und Dateien effizienter bearbeiten und aufbereiten. Experimentiere mit verschiedenen regulären Ausdrücken und finde heraus, welche Ergebnisse du erzielen kannst!