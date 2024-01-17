---
title:                "Ermittlung der Länge eines Strings"
html_title:           "PHP: Ermittlung der Länge eines Strings"
simple_title:         "Ermittlung der Länge eines Strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge einer Zeichenkette in einem PHP-Programm zu finden, bedeutet herauszufinden, wie viele Zeichen sich in dieser Zeichenkette befinden. Programmierer tun dies, um verschiedene Aufgaben auszuführen, wie zum Beispiel das Validieren von Benutzereingaben oder das Manipulieren von Texten.

## Wie man es macht:
Die Länge einer Zeichenkette in PHP zu finden ist einfach. Verwende einfach die vordefinierte Funktion `strlen()`, die die Anzahl der Zeichen in der Zeichenkette zurückgibt. Hier ist ein Beispielcode, der eine Zeichenkette durch Variablensubstitution zuweist und dann die Länge dieser Zeichenkette mithilfe von `strlen()` ausgibt:

```PHP
$string = "Hallo Welt!";
echo strlen($string); 
```

Die Ausgabe wird `11` sein, da die Zeichenkette 11 Zeichen enthält (einschließlich Leerzeichen).

## Tiefgehende Informationen:
Die Funktion `strlen()` wurde bereits in der ersten Version von PHP im Jahr 1997 eingeführt. Es gibt auch alternative Methoden, um die Länge einer Zeichenkette zu finden, wie zum Beispiel die Funktion `mb_strlen()`, die mit mehrsprachigen Zeichenkodierungen umgehen kann. Die Implementierung von `strlen()` in PHP ist jedoch sehr effizient, da sie die Länge ohne Schleifen oder andere komplexe Berechnungen bestimmt.

## Siehe auch:
- Dokumentation für die Funktion `strlen()`: https://www.php.net/manual/de/function.strlen.php
- Alternative Methode: `mb_strlen()`: https://www.php.net/manual/de/function.mb-strlen.php
- Erklärung zur String-Manipulation in PHP: https://www.tutorialrepublic.com/php-tutorial/php-string-manipulation.php