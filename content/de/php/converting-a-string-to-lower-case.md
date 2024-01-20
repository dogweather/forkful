---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Strings in Kleinbuchstaben ist ein Vorgang, bei dem alle Großbuchstaben in einem Textstring durch ihre Kleinbuchstabenequivalente ersetzt werden. Programmierer tun dies oft, um die Konsistenz der Daten zu gewährleisten und die Textvergleiche zu vereinfachen.

## Wie geht das:
PHP bietet eine eingebaute Funktion `strtolower()`, um einen String in Kleinbuchstaben umzuwandeln. Hier ist ein einfaches Beispiel:

```PHP
<?php
   $text = 'HELLO, WORLD!';
   echo strtolower($text);
?>
```
Dies gibt `hello, world!` aus.

Die Verwendung der Funktion ist sehr einfach. Sie akzeptiert einen String als Parameter und gibt seine Kleinbuchstabenversion zurück.

## Tiefer Verständnis:
In der Vergangenheit, bevor die Funktion `strtolower()` eingeführt wurde, musste man einen vollständigen manuellen Ansatz verwenden, um eine solche Aufgabe zu erfüllen. Dies war oft fehleranfällig und ineffizient.

Es gibt auch alternative Ansätze, um einen String in Kleinbuchstaben umzuwandeln, z. B. die Verwendung von `mb_strtolower()`, die für multibyte Strings besser geeignet ist (wie solche, die Sonderzeichen oder Nicht-Englisch-Zeichen enthalten). 

Was die Implementierung betrifft, so verändert die Funktion `strtolower()` nicht den Originalstring selbst, sondern gibt eine neue transformierte Version zurück, was bedeutet, dass sie sicher in jeder Situation verwendet werden kann, ohne befürchten zu müssen, dass sie die Ursprungsdaten verändert.

## Siehe auch:
Für weitere Informationen und Details über die Funktion `strtolower()`, besuchen Sie die offizielle PHP-Dokumentation (https://www.php.net/manual/en/function.strtolower.php).

Für die Behandlung von mehrsprachigen Strings gibt es auch die Funktion `mb_strtolower()` (https://www.php.net/manual/en/function.mb-strtolower.php). 

Um die Möglichkeiten der Textmanipulation in PHP weiter zu erkunden, beachten Sie auch die Funktionen `strtoupper()` (https://www.php.net/manual/en/function.strtoupper.php) und `ucwords()` (https://www.php.net/manual/en/function.ucwords.php).