---
title:                "Erzeugen von Zufallszahlen"
html_title:           "PHP: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Manchmal gibt es in der Programmierung Situationen, in denen man zufällige Zahlen oder Daten benötigt. Zum Beispiel beim Erstellen von Passwörtern, Lottozahlen oder beim Testen von Algorithmen. PHP bietet verschiedene Funktionen an, um zufällige Nummern oder Werte zu generieren.

## Wie geht's

Um zufällige Nummern oder Werte in PHP zu generieren, gibt es mehrere Funktionen, die wir verwenden können. Eine davon ist `rand()`, die eine zufällige Ganzzahl zwischen zwei angegebenen Werten zurückgibt. 

```PHP
// Generiert eine zufällige Zahl zwischen 1 und 10
$random_number = rand(1, 10);
echo $random_number; // Output: z.B. 7
```

Eine weitere nützliche Funktion ist `mt_rand()`, die eine zufällige Ganzzahl basierend auf dem Mersenne-Twister-Algorithmus zurückgibt. Dieser Algorithmus gilt als besonders gut geeignet für die Erzeugung von zufälligen Werten.

```PHP
// Generiert eine zufällige Zahl zwischen 100 und 200
$random_number = mt_rand(100, 200);
echo $random_number; // Output: z.B. 126
```

Es gibt auch die Möglichkeit, zufällige Fließkommazahlen mit der Funktion `mt_rand()` zu generieren. Diese gibt jedoch keine gleichmäßig verteilten Zahlen zurück, sondern folgt einer speziellen Wahrscheinlichkeitsverteilung. Alternativ können wir die Funktion `lcg_value()` verwenden, die gleichmäßig verteilte zufällige Fließkommazahlen zurückgibt.

## Eintauchen

Bei der Erzeugung von zufälligen Zahlen oder Werten ist es wichtig zu beachten, dass diese Werte nicht wirklich zufällig sind, sondern auf mathematischen Algorithmen basieren. Daher sollten sie nicht für sicherheitsrelevante Anwendungen wie Passwörter oder Verschlüsselung verwendet werden.

PHP bietet auch weitere Funktionen an, um spezifische Anforderungen bei der Generierung von Zufallszahlen zu erfüllen. Dazu gehören `array_rand()`, um zufällige Elemente aus einem Array zurückzugeben, und `shuffle()`, um die Reihenfolge von Elementen in einem Array zufällig zu ändern.

Eine weitere interessante Funktion ist `random_bytes()`, mit der man sicherere zufällige Bytes generieren kann, die für kryptografische Anwendungen geeignet sind. Diese stellt sicher, dass die Zahlen wirklich zufällig sind und nicht durch mathematische Algorithmen vorhersehbar.

## Siehe auch

- [Offizielle PHP-Dokumentation zu Zufallszahlen](https://www.php.net/manual/de/book.random.php)
- [Artikel zu Zufallszahlen in PHP von TutorialsPoint](https://www.tutorialspoint.com/php/php_random_numbers.htm)
- [Weitere nützliche PHP-Funktionen im CPC-Code-Club](https://thecpc.codeclub.org.uk/php/functions)