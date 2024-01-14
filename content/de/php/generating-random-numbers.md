---
title:                "PHP: Zufallszahlen generieren"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiges Element in der Programmierung von Spielen, Lotteriesystemen oder auch Verschlüsselungsmechanismen. Mit PHP können Sie ganz einfach Zufallszahlen generieren, die für verschiedene Anwendungen nützlich sind.

## Wie

Um in PHP Zufallszahlen zu generieren, verwenden Sie die Funktion `rand()`, die zwei Parameter erwartet: die minimale und die maximale Zahl, zwischen denen die Zufallszahl liegen soll. Zum Beispiel, um eine Zufallszahl zwischen 1 und 10 zu generieren, können Sie den folgenden Code verwenden:

```PHP
$zufallszahl = rand(1, 10);
echo $zufallszahl; //Gibt eine Zufallszahl zwischen 1 und 10 aus
```

Sie können auch Arrays mit `rand()` verwenden, um einen zufälligen Index oder Wert aus dem Array auszuwählen. Das folgende Beispiel demonstriert, wie man einen zufälligen Monat aus einem Array von Monaten auswählt:

```PHP
$monate = array("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember");
$zufall_monat = $monate[rand(0, 11)];
echo $zufall_monat; //Gibt einen zufälligen Monat aus dem Array aus
```

## Deep Dive

Die Funktion `rand()` verwendet den linearen Kongruenz Generator, um Zufallszahlen zu generieren. Dieser Algorithmus verwendet eine mathematische Formel, um vorhersehbare Zahlenreihen zu erzeugen, die als Zufallszahlen verwendet werden können. Es gibt jedoch einige Einschränkungen, da die generierten Zahlen nicht wirklich zufällig sind, sondern vorhersehbar sind. Aus diesem Grund sollten Sie `rand()` nicht für Sicherheitszwecke verwenden.

Eine bessere Alternative für Sicherheitszwecke ist die Funktion `random_int()`, die auf Kryptographie-geprüften Zufallszahlen basiert. Diese Funktion verwendet das PHP Extension Cryptographic library (Libsodium), die eine bessere Zufälligkeit für Anwendungen bietet, die zufällige Daten benötigen.

## Siehe auch

- Offizielle Dokumentation zu `rand()`: https://www.php.net/manual/en/function.rand.php
- Offizielle Dokumentation zu `random_int()`: https://www.php.net/manual/en/function.random-int.php
- Weitere Infos zu Zufallszahlen in PHP: https://www.php.net/manual/en/intro.random.php