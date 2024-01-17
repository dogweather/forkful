---
title:                "Zufällige Zahlen generieren"
html_title:           "PHP: Zufällige Zahlen generieren"
simple_title:         "Zufällige Zahlen generieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
 Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung, da sie es uns ermöglicht, auf vielfältige Weise verschiedene Ergebnisse zu erhalten. Diese Zahlen dienen als Grundlage für viele Anwendungen wie Spiele, Verschlüsselung und Simulationen.

## Wie geht's?
Um in PHP Zufallszahlen zu generieren, können wir die ```rand()``` Funktion verwenden. Diese Funktion akzeptiert zwei Parameter: die untere und obere Grenze des Bereichs, aus dem die Zufallszahl generiert werden soll. Zum Beispiel können wir mit ```rand(1, 10)``` eine Zufallszahl zwischen 1 und 10 erhalten.

Eine weitere Möglichkeit ist die Nutzung der ```mt_rand()``` Funktion, die mithilfe des Mersenne Twister-Algorithmus hohe Qualität gewährleistet. Diese Funktion akzeptiert ebenfalls zwei Parameter, jedoch können wir hier auch negative Zahlen als Bereichsgrenzen angeben.

Es gibt auch die Möglichkeit, eine Zufallszahl mit der Funktion ```random_int()``` zu generieren, die die Sicherheit beim Generieren von Zufallszahlen verbessert.

Ein Beispiel für die Verwendung dieser Funktionen und die Ausgabe der generierten Zufallszahl könnte so aussehen:

```PHP
// Verwendung von rand()
$randomNumber1 = rand(1, 10);
echo $randomNumber1; // gibt eine Zufallszahl zwischen 1 und 10 aus

// Verwendung von mt_rand()
$randomNumber2 = mt_rand(-50, 50);
echo $randomNumber2; // gibt eine Zufallszahl zwischen -50 und 50 aus

// Verwendung von random_int()
$randomNumber3 = random_int(100, 200);
echo $randomNumber3; // gibt eine sichere Zufallszahl zwischen 100 und 200 aus
```

## Tiefere Einblicke
Die Verwendung von Zufallszahlen hat eine lange Geschichte in der Programmierung. In der Vergangenheit wurden oft Pseudozufallszahlen verwendet, die auf einem Algorithmus basieren und nur eine begrenzte Anzahl von möglichen Ergebnissen haben. Mit den neueren Funktionen wie ```mt_rand()``` und ```random_int()``` haben wir jedoch Zugriff auf hochwertige Zufallszahlen, die sicherer und vielseitiger sind.

Wenn du auf der Suche nach alternativen Methoden zur Generierung von Zufallszahlen bist, könntest du dir auch die Funktionen ```shuffle()``` und ```array_rand()``` anschauen, die es ermöglichen, zufällige Elemente aus einem Array auszuwählen.

Bei der Generierung von Zufallszahlen ist es wichtig zu bedenken, dass sie für sicherheitsrelevante Anwendungen wie Verschlüsselung nicht geeignet sind. Hier sollten andere Methoden wie Kryptographie-Tools verwendet werden.

## Siehe auch
- Die offizielle PHP-Dokumentation für die ```rand()``` Funktion: https://www.php.net/manual/en/function.rand.php
- Weitere Informationen zu den Zufallszahlenfunktionen in PHP: https://www.php.net/manual/en/ref.math.php