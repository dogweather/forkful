---
title:                "Die Länge eines Strings ermitteln"
html_title:           "PHP: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende Fähigkeit, die jeder Programmierer beherrschen sollte. Es ermöglicht es, die Größe einer Zeichenfolge zu bestimmen, was bei der Verarbeitung von Benutzereingaben oder der Datenbankabfrage unerlässlich ist.

## Wie geht es

Um die Länge einer Zeichenkette in PHP zu finden, können wir die folgende Funktion verwenden:

```PHP
$string = "Hallo Welt!";
echo strlen($string);
```

Dieses Beispiel zeigt, wie die Funktion `strlen` benutzt wird, um die Länge der Zeichenkette "Hallo Welt!" zu finden. Sie gibt die Anzahl der Zeichen in der Zeichenkette zurück, in diesem Fall 11.

Eine weitere Möglichkeit ist die Verwendung der Funktion `mb_strlen`, die für mehrbytefähige Zeichenketten wie z.B. Unicode-Zeichen besser geeignet ist. Hier ist ein Beispiel:

```PHP
$string = "äöü";
echo mb_strlen($string, 'UTF-8');
```

Dieses Beispiel verwendet die Funktion `mb_strlen`, um die Länge der Zeichenkette "äöü" zu finden. Beachten Sie, dass wir hier als zweites Argument die Zeichenkodierung angeben, um sicherzustellen, dass sie für mehrbytefähige Zeichen richtig berechnet wird.

## Deep Dive

Wie funktioniert die `strlen` Funktion überhaupt? Im Grunde genommen durchläuft sie jede einzelne Zeichen der Zeichenkette und zählt sie. Dabei wird jedoch nicht nur das sichtbare Zeichen gezählt, sondern auch Leerzeichen und unsichtbare Zeichen wie z.B. Zeilenumbrüche.

Es ist auch wichtig zu beachten, dass die Funktion `strlen` die Länge der Zeichenkette in Bytes zählt, nicht in Zeichen. Dies kann zu unerwarteten Ergebnissen führen, wenn die Zeichenfolge mehrbytefähige Zeichen enthält.

Die `mb_strlen` Funktion hingegen verwendet eine komplexere Logik, um die Länge der Zeichenkette zu bestimmen. Sie ist in der Lage, mehrbytefähige Zeichen korrekt zu zählen und kann auch mit verschiedenen Zeichenkodierungen umgehen.

Insgesamt ist es wichtig, zu verstehen, wie diese Funktionen arbeiten und wann man welche Funktion am besten einsetzt, um die Länge einer Zeichenkette in PHP zu finden.

## Siehe auch

- [PHP manual: String length](https://www.php.net/manual/en/function.strlen.php)
- [PHP manual: Multi-byte String functions](https://www.php.net/manual/en/book.mbstring.php)