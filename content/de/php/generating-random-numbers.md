---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:32.920855-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Randomzahlen generieren ist wie Würfeln, aber digital: Dein Programm kreiert Zahlen, die nicht vorhersehbar sind. Das ist wichtig für Sicherheit (Denk an Passwörter), Spiele (Wer will schon vorher wissen, was passiert?) und Simulationen (Wie im echten Leben, wo alles zufällig scheint).

## How to:

In PHP kannst Du ganz einfach loslegen:

```PHP
<?php
// Einfache Zufallszahl zwischen 0 und getrandmax()
echo rand() . "\n";

// Zufallszahl zwischen 10 und 50
echo rand(10, 50) . "\n";

// Mit mt_rand() für eine bessere Performance und Zufälligkeit
echo mt_rand() . "\n";

// Seit PHP 7.0 verwende random_int() für Kryptographie-Sicherheit
echo random_int(1, 100) . "\n";
?>
```

Beispiel-Ausgaben könnten sein:

```
310352758
29
1073741824
83
```

## Deep Dive:

Die Geschichte der Zufallszahlen in PHP hat sich entwickelt. Früher gab's `rand()`, aber das war nicht optimal. Mit PHP 4 kam `mt_rand()`, basiert auf dem Mersenne Twister Algorithmus – schneller und randomer. Seit PHP 7 ist `random_int()` die beste Wahl für Sicherheit wegen Kryptographie-Standards.

Andere Funktionen wie `openssl_random_pseudo_bytes()` geben Dir binäre Daten, aber `random_int()` ist meistens die einfachere Option.

Beim Implementieren auch beachten: Zufallszahlen sind oft nicht wirklich "zufällig", sondern von Algorithmen generiert. Perfekt zufällig sind sie selten. Aber für die meisten Anwendungen sind sie "zufällig genug".

## See Also:

- PHP Manual für `rand()` Funktion: https://www.php.net/manual/de/function.rand.php
- PHP Manual für `mt_rand()` Funktion: https://www.php.net/manual/de/function.mt-rand.php
- PHP Manual für `random_int()` Funktion: https://www.php.net/manual/de/function.random-int.php
- Mehr zum Mersenne Twister Algorithmus: https://de.wikipedia.org/wiki/Mersenne-Twister