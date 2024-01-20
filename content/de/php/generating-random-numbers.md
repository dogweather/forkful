---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen werden in der Programmierung häufig zum Simulieren von Unvorhersehbarkeit oder zum Testen von Funktionen verwendet. Sie sind ein wesentlicher Bestandteil von Spielen, sicherheitskritischen Anwendungen und mehr.

## So geht's:

In PHP können Sie mit der Funktion `rand()` oder `mt_rand()` Zufallszahlen generieren.

```PHP
<?php
$zufallszahl = rand(0, 10);
echo 'Zufallszahl zwischen 0 und 10: ' . $zufallszahl . PHP_EOL;

$zufallszahl = mt_rand(0, 100);
echo 'Zufallszahl zwischen 0 und 100: ' . $zufallszahl . PHP_EOL;
?>
```
Ausgabe könnte ähnlich sein wie:

```
Zufallszahl zwischen 0 und 10: 6
Zufallszahl zwischen 0 und 100: 83
```

## Tiefgehende Informationen

Historisch gaben frühere Versionen von PHP oft Voreingenommenheiten bei der Zahlenverteilung auf, was zu Sicherheitsproblemen führen konnte. Seit PHP 7.0 wird jedoch empfohlen, `random_int()` zu verwenden, welche eine kryptographisch sichere Zahl generiert.

Alternativen sind die Funktionen `random_bytes()` für die Generierung von zufälligen Bytes oder die Nutzung von `openssl_random_pseudo_bytes()`, wenn OpenSSL installiert ist.

Die Implementierung der zufälligen Zahlengenerierung in PHP variiert je nach der verwendeten Funktion, aber alle zielen darauf ab, eine gleichmäßige Verteilung zu erzeugen und Bias zu minimieren.

## Siehe auch

1. PHP-Dokumentation für `rand()`: <https://www.php.net/manual/de/function.rand.php>
2. PHP-Dokumentation für `mt_rand()`: <https://www.php.net/manual/de/function.mt-rand.php>
3. PHP-Dokumentation für `random_int()`: <https://www.php.net/manual/de/function.random-int.php>
4. PHP-Dokumentation für `random_bytes()`: <https://www.php.net/manual/de/function.random-bytes.php>
5. PHP-Dokumentation für `openssl_random_pseudo_bytes()`: <https://www.php.net/manual/de/function.openssl-random-pseudo-bytes.php>