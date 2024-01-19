---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# Artikel: Textabschnitte in PHP extrahieren

## Was & Warum?

Das Extrahieren von Textabschnitten ist der Prozess, einen bestimmten Textteil aus einem größeren String herauszuschneiden. Das ist essentiell, wenn man bestimmte Informationen von Nutzereingaben oder Datenbankausgaben extrahieren will.

## Wie geht's?

Das Extrahieren von Strings in PHP ist sehr einfach. Man benutzt die funktion `substr()`. Hier ist ein Beispiel:

```PHP 
<?php
$text = "Ich liebe Computerprogrammierung";
$teil = substr($text, 5, 6);

echo $teil;
?>
```

Die Ausgabe wird `liebe` sein.

## Tiefere Erkenntnisse

Historisch gesehen liegt die `substr()` Funktion von PHP im Kern vieler Anwendungen und Spiele. Es hat zwar Alternativen wie `mb_substr()` für multibyte Strings - etwa die UTF-8 Kodierung, ist aber immer noch die gebräuchlichste Funktion. Die Implementierung von `substr()` ist sehr effizient, sie verwendet C-style Zeichenketten und arbeitet mit `char *` Zeigern, nicht mit PHP's String-Datentyp.

## Siehe auch

Weitere nützliche Funktionen, mit denen Sie weiter experimentieren können, sind `strstr()`, `stristr()`, und `strpos()`. Die PHP-Dokumentation ist der beste Ort, um mehr zu erfahren. Hier sind die Links:

- [substr() Funktion - PHP Dokumentation ](https://www.php.net/manual/en/function.substr.php)
- [strstr() Funktion - PHP Dokumentation](https://www.php.net/manual/de/function.strstr.php)
- [stristr() Funktion - PHP Dokumentation](https://www.php.net/manual/de/function.stristr.php)
- [strpos() Funktion - PHP Dokumentation](https://www.php.net/manual/de/function.strpos.php) 

Fang an, mit ihnen zu spielen und du wirst sehen, wie mächtig sie sein können!