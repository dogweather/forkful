---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (Regex) sind Muster, welche zum Suchen und Ersetzen in Strings verwendet werden. Sie sind extrem leistungsstark und flexibel, um präzise Textmanipulationen und komplexe Suchvorgänge durchzuführen.

## How to:
Regex in PHP nutzt die `preg_*` Funktionen. Hier sind einfache Beispiele:

```PHP
// Prüfen, ob ein String eine E-Mail-Adresse enthält
$emailPattern = '/[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}/';
$text = "Meine E-Mail ist beispiel@example.com.";
if (preg_match($emailPattern, $text, $matches)) {
    echo "Gefunden: " . $matches[0]; // Output: Gefunden: beispiel@example.com
}

// Ersetzen aller Zahlen durch '#'
$numberPattern = '/\d+/';
$string = "Ich habe 100 Äpfel und 10 Bananen.";
$replacement = preg_replace($numberPattern, '#', $string);
echo $replacement; // Output: Ich habe # Äpfel und # Bananen.
```

## Deep Dive
Regex gibt es seit den 1950er Jahren und wurde in den 60ern durch Ken Thompson in Texteditoren eingeführt. Alternativen zu Regex sind String-Funktionen wie `strpos` oder `str_replace`, die schneller sein können, aber weniger mächtig. Reguläre Ausdrücke in PHP verwenden die PCRE (Perl Compatible Regular Expressions) Bibliothek, die komplexere Pattern erlaubt als POSIX Regex.

## See Also
- PHP Manual on PCRE: https://www.php.net/manual/en/book.pcre.php
- Regex Tester Online: https://regex101.com/
- Regex Crash-Kurs: https://www.regular-expressions.info/tutorial.html