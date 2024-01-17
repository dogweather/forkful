---
title:                "Ein String in Kleinbuchstaben umwandeln"
html_title:           "PHP: Ein String in Kleinbuchstaben umwandeln"
simple_title:         "Ein String in Kleinbuchstaben umwandeln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Konvertieren von einer Zeichenfolge in Kleinbuchstaben bedeutet, alle Buchstaben in einer Zeichenfolge von Großbuchstaben in Kleinbuchstaben umzuwandeln. Programmierer verwenden diese Funktion, um Zeichenfolgen zu standardisieren und eine einheitliche Formatierung zu gewährleisten.

# Wie geht's?
Ein Beispiel für die Verwendung der Funktion `strtolower()` in PHP:

```
$string = "Guten Morgen, Programmierer!";
echo strtolower($string);
```

Dieses Beispiel gibt die Ausgabe "guten morgen, programmierer!" zurück.

# Tief tauchen
Historischer Kontext: Das Konzept der Groß- und Kleinschreibung stammt aus der Zeit der Handschriften und der Drucktechnik. In der Programmierung sind Zeichenfolgen jedoch nicht case-sensitive und Groß- und Kleinschreibung haben keine Auswirkungen auf die Funktionalität. Die Verwendung von Kleinbuchstaben kann jedoch helfen, die Lesbarkeit von Code zu verbessern.

Alternativen: Eine Alternative zur Verwendung der `strtolower()` Funktion in PHP ist die Verwendung von regulären Ausdrücken, um Zeichenfolgen zu formatieren.

Implementierungsdetails: Die `strtolower()` Funktion nutzt die Unicode-Einstellungen des Servers, um Buchstaben in Kleinbuchstaben umzuwandeln. Dadurch wird sichergestellt, dass auch Sonderzeichen oder Umlaute korrekt konvertiert werden.

# Siehe auch
Offizielle PHP-Dokumentation für [strtolower()](https://www.php.net/manual/en/function.strtolower.php)

StackOverflow-Diskussion über [Case-Insensitivität in der Programmierung](https://stackoverflow.com/questions/2364925/case-insensitive-programming-languages)