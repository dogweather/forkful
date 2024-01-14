---
title:    "PHP: Extrahieren von Teilen einer Zeichenkette"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Die Extraktion von Teilstrings ist eine häufige Aufgabe bei der Programmierung in PHP. Sie ermöglicht es uns, bestimmte Teile von Zeichenketten (Strings) zu isolieren und damit besser manipulieren und verarbeiten zu können. In diesem Blogbeitrag werde ich erklären, warum es wichtig ist, die Funktionen zur Extraktion von Teilstrings zu beherrschen und wie man sie in der Praxis anwenden kann.

## Wie

Um Teilstrings aus einer Zeichenkette zu extrahieren, können wir in PHP die Funktion `substr()` verwenden. Diese Funktion erwartet als ersten Parameter die Zeichenkette, aus der der Teilstring extrahiert werden soll, und als zweiten Parameter eine Startposition, ab der der Teilstring beginnen soll. Optional kann noch ein dritter Parameter angegeben werden, der die Länge des Teilstrings festlegt. Hier ein Beispiel:

```PHP
$string = "Hello World!";
$subString1 = substr($string, 0, 5); // Ausgabe: "Hello"
$subString2 = substr($string, 6); // Ausgabe: "World!"
```

Wenn kein dritter Parameter angegeben wird, wird die Funktion alle Zeichen ab der angegebenen Startposition bis zum Ende der Zeichenkette zurückgeben. Die Funktion `mb_substr()` kann verwendet werden, um Mehrbyte-Zeichen zu verarbeiten.

## Deep Dive

Um noch tiefer in das Thema einzutauchen, schauen wir uns die UTF-8-Kodierung genauer an. Diese Kodierung ist in PHP standardmäßig eingestellt und kann Auswirkungen auf die Extraktion von Teilstrings haben. Wenn wir beispielsweise einen Teilstring aus einem mehrbyte-Zeichen extrahieren möchten, müssen wir die Position des Zeichens berücksichtigen. Hier ist ein Beispiel:

```PHP
$string = "Füße";
$subString = substr($string, 0, 2); // Ausgabe: "Fü"
$subString = mb_substr($string, 0, 5); // Ausgabe: "Füße"
```

Wie wir sehen, müssen wir bei der Verwendung von `substr()` die Startposition basierend auf der Anzahl der Bytes des Zeichens auswählen, während `mb_substr()` die Anzahl der Zeichen erwartet.

## Siehe auch

- PHP Dokumentation zu substr(): https://www.php.net/manual/de/function.substr.php
- PHP Dokumentation zu mb_substr(): https://www.php.net/manual/de/function.mb-substr.php