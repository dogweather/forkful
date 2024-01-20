---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Der String Interpolation: Was und Warum?

Eine Zeichenkette zu interpolieren bedeutet, Variablenwerte direkt in eine Zeichenkette einzubetten. In der PHP-Programmierung macht dies unseren Code sauberer und lesbarer, indem es die Notwendigkeit von vielen Konkatenierungsoperatoren entfernt. 

# Wie geht das?

Hier ist ein einfacher Weg, wie Sie einen String in PHP interpolieren. Ersetzen Sie einfach die Platzhalter in der Zeichenkette durch die tatsächlichen Variablenwerte.

```PHP
$name = "John";
echo "Hallo, $name";

// Output: Hallo, John
```
Wie Sie sehen können, ist der Code lesbar und leicht zu verstehen. 

# Deep Dive

Zur historischen Kontext: Die Zeichenketteninterpolation gibt es seit den frühen Tagen von PHP. Es ist ein Merkmal, das PHP von anderen Sprachen wie C unterscheidet, die keine native Unterstützung für die Zeichenketteninterpolation haben.

Einige Alternativen zur Zeichenketteninterpolation in PHP sind der Konkatenierungsoperator (.) und die sprintf()-Funktion. Beide Techniken können auch zur Kombination von Zeichenketten und Variablen verwendet werden, aber sie sind in der Regel weniger lesbar und weniger effizient.

Im Hinblick auf die Implementierung ist zu beachten, dass die Interpolation nur in doppelten Anführungszeichen funktioniert, nicht in einfachen. Daher ist der folgende Code ungültig:

```PHP
$name = "John";
echo 'Hallo, $name'; // Falsch, da einfache Anführungszeichen verwendet werden

// Output: Hallo, $name
```
# Siehe Auch 

- PHP-Handbuch zu Zeichenketten: [Link](https://www.php.net/manual/de/language.types.string.php)
- PHP: The Right Way – Interpolation: [Link](https://phptherightway.com/#interpolation)