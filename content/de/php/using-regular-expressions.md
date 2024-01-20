---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "PHP: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind ein nützliches Werkzeug beim Programmieren, das es Programmierern ermöglicht, Textmuster auf einfache und effiziente Weise zu finden und zu verarbeiten. Sie sind besonders nützlich bei der Validierung von Benutzereingaben oder beim Durchsuchen von Textdateien.

## Wie geht's?
Das folgende Beispiel zeigt, wie man mit PHP reguläre Ausdrücke verwenden kann, um eine E-Mail-Adresse zu validieren und den Benutzer darüber zu informieren:

```PHP
$email = "beispiel@example.com";

if (preg_match("/^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$/i", $email)) {
    echo "Die E-Mail-Adresse ist gültig.";
} else {
    echo "Die E-Mail-Adresse ist ungültig.";
}
```

Die Ausgabe wäre in diesem Fall "Die E-Mail-Adresse ist gültig."

## Tiefentauchen
Reguläre Ausdrücke wurden bereits in den 1950er Jahren durch die Theorie der formalen Sprachen entwickelt und haben seitdem in vielen Programmiersprachen Verwendung gefunden. Alternativen zu regulären Ausdrücken sind zum Beispiel String-Funktionen oder Parsing-Bibliotheken.

Die Verwendung von regulären Ausdrücken in PHP erfolgt hauptsächlich über die `preg_` Funktionen, die speziell für diesen Zweck entwickelt wurden. Diese Funktionen bieten verschiedene Optionen und Modifikatoren, um die Suche und Verarbeitung von Textmustern zu optimieren.

## Siehe auch
Für weitere Informationen zu regulären Ausdrücken in PHP können folgende Quellen hilfreich sein:

- [Offizielle Dokumentation von PHP zu regulären Ausdrücken](https://www.php.net/manual/de/reference.pcre.pattern.syntax.php)
- [Reguläre Ausdrücke bei Wikipedia](https://de.wikipedia.org/wiki/Regul%C3%A4rer_Ausdruck)