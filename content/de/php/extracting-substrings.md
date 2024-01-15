---
title:                "Auslesen von Teilzeichenketten"
html_title:           "PHP: Auslesen von Teilzeichenketten"
simple_title:         "Auslesen von Teilzeichenketten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich mit dem Extrahieren von Teilstrings in PHP beschäftigen sollte. Vielleicht möchtest du Daten aus einer Zeichenkette extrahieren, um sie in einem bestimmten Format anzuzeigen, oder du möchtest Benutzereingaben überprüfen, um sicherzustellen, dass sie den gewünschten Kriterien entsprechen. Egal aus welchem Grund, das Extrahieren von Teilstrings ist eine wichtige Fähigkeit, die jeder PHP-Programmierer beherrschen sollte.

## Wie geht man vor?

Um einen Teilstring in PHP zu extrahieren, gibt es verschiedene Funktionen und Methoden, die man verwenden kann. Eine Möglichkeit ist die Verwendung der ```substr```-Funktion, die es dir ermöglicht, einen Teilstring aus einer bestimmten Zeichenkette auszuschneiden. Sie akzeptiert drei Parameter: Die Eingangszeichenkette, den Startindex und die gewünschte Länge des Teilstrings.

Zum Beispiel, wenn wir aus der Zeichenkette "Hallo Welt!" den Teilstring "Welt" extrahieren möchten, verwenden wir folgenden Code:

```PHP
$string = "Hallo Welt!";
$teilstring = substr($string, 6, 4);
echo $teilstring;
// Ausgabe: Welt
```

Eine weitere nützliche Funktion ist ```strpos```, die uns den Index des ersten Vorkommens eines bestimmten Zeichens oder Teilstrings innerhalb einer Zeichenkette gibt. Mit dieser Funktion können wir zum Beispiel überprüfen, ob ein bestimmter Teilstring in einer Benutzereingabe vorhanden ist.

```PHP
$string = "Ich mag Schokolade!";
if (strpos($string, "Schokolade") !== false) {
  echo "Ja, ich mag Schokolade!";
} else {
  echo "Ich mag keine Schokolade!";
}
// Ausgabe: Ja, ich mag Schokolade!
```

## Tiefer tauchen

Beim Extrahieren von Teilstrings gibt es noch weitere Dinge zu beachten, wie zum Beispiel die Behandlung von Unicode-Zeichen und die Verwendung von regulären Ausdrücken. Hier ist es wichtig, sich mit den verschiedenen Funktionen und Methoden vertraut zu machen und zu verstehen, wie sie in verschiedenen Szenarien verwendet werden können.

Ein häufiges Problem beim Extrahieren von Teilstrings ist auch die Beachtung der Spracheinstellungen. Je nach Spracheinstellung kann die Indexierung von Zeichen in einer Zeichenkette unterschiedlich sein, was zu unerwarteten Ergebnissen führen kann. Deshalb ist es wichtig, alle Funktionen und Methoden sorgfältig zu überprüfen und gegebenenfalls die Spracheinstellungen anzupassen.

## Siehe auch

- Offizielle PHP-Dokumentation zu [substr](https://www.php.net/manual/en/function.substr.php) und [strpos](https://www.php.net/manual/en/function.strpos.php)
- Ein Tutorial von TutsPlus über [das Extrahieren von Teilstrings in PHP](https://code.tutsplus.com/tutorials/manipulating-strings-in-php--net-21689)