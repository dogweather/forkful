---
title:                "Einen String großschreiben"
html_title:           "PHP: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# String großschreiben in PHP

## Was & Warum?

Ein String in Großbuchstaben zu konvertieren bedeutet, jeden Buchstaben im String in seine Großbuchstabenform zu ändern. Programmierer tun dies, um Nutzereingaben zu standardisieren oder Text hervorzuheben.

## Anleitung:

In PHP verwenden wir die Funktion `strtoupper()`, um einen String zu kapitalisieren. Schauen wir uns ein Beispiel an:

```PHP
$text = "hallo welt!";
$capitalizedText = strtoupper($text);
echo $capitalizedText;
```

Ausgabe:

```PHP
"HALLO WELT!"
```
So einfach ist das! Geben Sie Ihren String an `strtoupper()` weiter und der Text wird vollständig in Großbuchstaben umgewandelt.

## Tiefgehende Details:

Die Funktion `strtoupper()` ist seit PHP 4 verfügbar und hat eine hohe Leistung. PHP bietet auch alternative Funktionen wie `ucwords()` oder `ucfirst()`, welche die erste Buchstaben jedes Wortes bzw. des gesamten Strings respektive in Großbuchstaben umwandeln. Beachten Sie, dass `strtoupper()` keine Auswirkungen auf Zahlen oder Sonderzeichen hat, nur auf Buchstaben.

Es ist auch wichtig zu wissen, dass `strtoupper()` Multibyte-Zeichenketten (MB: Multibyte) unterstützt. Wenn Sie Multibyte-Zeichen in Großbuchstaben umwandeln möchten, stellen Sie sicher, dass Sie die korrekte MB-Funktion, `mb_strtoupper()`, verwenden.

## Siehe auch:

Zum weiteren Studium:

- Die offizielle PHP-Dokumentation zu [`strtoupper()`](https://www.php.net/manual/de/function.strtoupper.php)
- Ein nützliches Tutorial auf [W3Schools](https://www.w3schools.com/php/func_string_strtoupper.asp)

Bitte verwenden Sie diese Ressourcen, um mehr über die Verwendung und Funktionalität von `strtoupper()` in PHP zu erfahren. Happy coding!