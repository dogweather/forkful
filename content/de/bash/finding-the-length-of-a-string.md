---
title:                "Bash: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Bestimmen der Länge einer Zeichenfolge ist eine häufige Aufgabe beim Programmieren in Bash. Es kann hilfreich sein, um zum Beispiel die Eingabe einer Benutzereingabe zu überprüfen oder um sicherzustellen, dass eine bestimmte Bedingung erfüllt ist. In diesem Blog-Beitrag werden wir uns ansehen, wie man die Länge einer Zeichenfolge in Bash berechnen kann und welche Aspekte dabei zu beachten sind.

## Wie geht man vor

Um die Länge einer Zeichenfolge in Bash zu finden, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung der integrierten Funktion `expr length`. Diese Funktion gibt die Anzahl der Zeichen in einer gegebenen Zeichenfolge zurück.

```Bash
# Beispielcode zur Bestimmung der Länge einer Zeichenfolge
string="Hallo Welt!"
echo "Die Länge der Zeichenfolge ist: $(expr length $string)"
```

Als Ausgabe erhalten wir:

```
Die Länge der Zeichenfolge ist: 11
```

Man kann auch die Variante `BASH_SUBSTR` von `expr` verwenden, um nur eine Teilzeichenfolge zu zählen. Dafür muss man die Anfangsposition und die Anzahl der zu zählenden Zeichen angeben.

```Bash
# Beispielcode zur Bestimmung der Länge einer Teilzeichenfolge
string="Hallo Welt!"
echo "Die Länge der Teilzeichenfolge ist: $((${#string}-5))"
```

Dieses Beispiel gibt uns als Ausgabe:

```
Die Länge der Teilzeichenfolge ist: 6
```

Es ist auch möglich, die Länge einer Zeichenfolge mit Hilfe der Shell-Parameterverarbeitung zu bestimmen. Dafür können wir die folgende Syntax verwenden:

```Bash
# Beispielcode zur Bestimmung der Länge einer Zeichenfolge mit Hilfe von Shell-Parameterverarbeitung
string="Hallo Welt!"
echo "Die Länge der Zeichenfolge ist: ${#string}"
```

Die Ausgabe ist dieselbe wie im ersten Beispiel:

```
Die Länge der Zeichenfolge ist: 11
```

Es gibt also verschiedene Möglichkeiten, die Länge einer Zeichenfolge in Bash zu finden. Je nach Bedarf kann man die passende Methode auswählen.

## Tieferer Einblick

Beim Bestimmen der Länge einer Zeichenfolge gibt es einige Aspekte zu beachten. Zum Beispiel kann es Unterschiede bei der Behandlung von Leerzeichen oder Sonderzeichen geben. Auch die Verwendung von Unicode-Zeichen kann zu unerwarteten Ergebnissen führen.

Ein weiterer wichtiger Punkt ist die Performance. Je nach Größe der Zeichenfolge und verwendeter Methode kann die Berechnung der Länge mehr oder weniger Zeit in Anspruch nehmen. Es ist daher ratsam, in Bezug auf Geschwindigkeit und Effizienz auch andere Faktoren zu berücksichtigen, bevor man die Länge einer Zeichenfolge bestimmt.

## Siehe auch

- [Bash-Referenzhandbuch](https://www.gnu.org/software/bash/manual/bash.html)
- [Linux Bash Guide for Beginners](https://linuxconfig.org/Linux_bash_scripting_Tutorial)
- [Online Bash Compiler](https://www.onlinegdb.com/online_bash_compiler)