---
title:                "PHP: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen oder Verkettung von Strings ist eine grundlegende Funktion in der Programmierung, die es ermöglicht, Texte und Variablen miteinander zu kombinieren. Es ist besonders nützlich, wenn wir dynamisch generierte Daten oder Nachrichten erstellen wollen.

## Wie man String-Verkettung in PHP durchführt

Eine der einfachsten Möglichkeiten, Strings in PHP zu verketten, ist die Verwendung des Konkatenationsoperators `.`. Hier ist ein Beispiel, wie man zwei Strings miteinander verketten kann:

```PHP
$vorname = "Max";
$nachname = "Mustermann";

echo $vorname . " " . $nachname;
// Ausgabe: Max Mustermann
```

Das `.` Zeichen kann auch verwendet werden, um Variablen und andere Datentypen miteinander zu verketten. Hier ist ein Beispiel, wie man eine Zahl zu einem String hinzufügt:

```PHP
$anzahl = 5;

echo "Ich habe " . $anzahl . " Äpfel.";
// Ausgabe: Ich habe 5 Äpfel.
```

Es ist auch möglich, mehrere Strings in einer Zeile zu verketten, indem man mehrere `.` Operatoren verwendet:

```PHP
$nachricht = "Hallo" . ", " . "wie geht es dir?";

echo $nachricht;
// Ausgabe: Hallo, wie geht es dir?
```

## Tiefergehende Informationen

Bei der Verkettung von Strings müssen wir auch auf die Datentypen achten. Wenn wir zwei Strings miteinander verketten, bleiben beide Strings vom Typ `string`. Wenn wir jedoch einen String mit einer Zahl verketten, wird die Zahl automatisch in einen String konvertiert. Das bedeutet, dass wir vorsichtig sein müssen, wenn wir Zahlen und andere Datentypen verketten, um unerwartete Ergebnisse zu vermeiden.

Eine weitere wichtige Sache, auf die wir achten müssen, ist die Verwendung der richtigen Anführungszeichen. Wenn wir Strings innerhalb von Anführungszeichen verketten, sollten wir sicherstellen, dass wir die richtigen Anführungszeichen verwenden, um nicht versehentlich den Code zu brechen.

## Siehe auch

- [PHP String-Verkettung](https://www.php.net/manual/en/language.operators.string.php)
- [PHP String-Funktionen](https://www.php.net/manual/en/ref.strings.php)
- [Grundlagen der Programmierung mit PHP](https://www.php.net/manual/en/getting-started.php)