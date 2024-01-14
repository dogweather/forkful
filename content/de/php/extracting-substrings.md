---
title:                "PHP: Unterstrings extrahieren"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substrings sind ein nützliches Konzept in der Programmierung, das uns ermöglicht, Teilstrings aus einem Text oder einer Zeichenkette auszuwählen. Dies kann uns helfen, Daten effizienter zu verarbeiten und bestimmte Muster in Strings zu identifizieren. In diesem Artikel werden wir uns damit beschäftigen, wie man mithilfe von PHP substrings aus einer Zeichenkette extrahiert.

## Wie geht's

Um einen Substring aus einer Zeichenkette zu extrahieren, verwenden wir die `substr()` Funktion in PHP. Diese Funktion benötigt zwei Parameter: die Zeichenkette, aus der wir den Substring extrahieren möchten, und die Position, ab der der Substring beginnen soll.

```php
$string = "Hallo Welt";
$substring = substr($string, 6);
echo $substring; // Ausgabe: Welt
```

In diesem Beispiel haben wir einen Substring von der Zeichenkette "Hallo Welt" extrahiert, der ab der 6. Stelle (Index beginnt bei 0) beginnt.

Wir können auch einen dritten Parameter hinzufügen, um die Länge des Substrings anzugeben.

```php
$string = "Hallo Welt";
$substring = substr($string, 6, 4);
echo $substring; // Ausgabe: Welt
```

In diesem Beispiel haben wir die Länge des Substrings auf 4 gesetzt, so dass der ausgegebene Substring nur aus den 4 folgenden Zeichen besteht.

Es ist auch möglich, negative Indizes zu verwenden, um von hinten beginnend zu zählen.

```php
$string = "Hallo Welt";
$substring = substr($string, -4);
echo $substring; // Ausgabe: Welt
```

In diesem Beispiel haben wir den Substring ab der 4. Stelle von hinten extrahiert.

## Tiefer Einblick

Neben der Verwendung von `substr()` gibt es auch andere Methoden, um Substrings in PHP zu extrahieren. Zum Beispiel können wir die `strrpos()` Funktion verwenden, um die Position des letzten Vorkommens eines Zeichens in einer Zeichenkette zu finden und dann `substr()` verwenden, um den Substring ab dieser Position zu extrahieren.

```php
$string = "Rot, Grün, Blau";
$position = strrpos($string, ",") + 2;
$substring = substr($string, $position);
echo $substring; // Ausgabe: Blau
```

Zusätzlich können wir auch mit regulären Ausdrücken arbeiten, um bestimmte Muster in einer Zeichenkette zu identifizieren und Substrings zu extrahieren.

Es ist wichtig zu beachten, dass die Verwendung von Substrings mit Vorsicht behandelt werden sollte, da sie dazu führen können, dass Daten unsauber oder unvollständig verarbeitet werden. Es ist daher ratsam, Methoden wie `strlen()` zu verwenden, um die Länge einer Zeichenkette zu überprüfen, bevor man Substrings extrahiert.

## Siehe auch

- [PHP substr() Funktion](https://www.php.net/manual/de/function.substr.php)
- [PHP strrpos() Funktion](https://www.php.net/manual/de/function.strrpos.php)
- [PHP reguläre Ausdrücke](https://www.php.net/manual/de/reference.pcre.pattern.syntax.php)