---
title:                "PHP: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum sollte man eine Zeichenfolge groß schreiben?

Das Großschreiben einer Zeichenfolge kann in verschiedenen Situationen sehr nützlich sein. Zum Beispiel könnte es erforderlich sein, einen Benutzernamen oder ein Passwort in Großbuchstaben zu speichern, um die Sicherheit zu verbessern. Auch beim Erstellen von formatierten Texten, wie Überschriften oder Zitaten, kann die Verwendung von Großbuchstaben ein ansprechenderes Erscheinungsbild erzeugen.

# Anleitung zum Großschreiben einer Zeichenfolge in PHP

```php
<?php
// Beispiel: Wir haben eine Variable mit dem Inhalt "hallo welt"
$string = "hallo welt";

// Mit der Funktion strtoupper() können wir die Zeichenfolge in Großbuchstaben umwandeln
$capitalized = strtoupper($string);

// Ausgabe des Ergebnisses
echo $capitalized;
// Ausgabe: HALLO WELT
?>
```

# Deeper Dive: Weitere Informationen zum Großschreiben von Zeichenfolgen

Es gibt mehrere Möglichkeiten, in PHP eine Zeichenfolge in Großbuchstaben zu konvertieren. Die Funktion `strtoupper()` ist die einfachste und in den meisten Fällen ausreichend. Es gibt jedoch auch die Funktion `ucwords()`, die die erste Buchstabe jedes Worts in einer Zeichenfolge groß schreibt. Beide Funktionen sind in PHP bereits vorinstalliert und können direkt genutzt werden.

Eine weitere wichtige Information ist, dass bei der Umwandlung in Großbuchstaben auch die Buchstaben mit Umlauten (ä,ö,ü) und Sonderzeichen (ß) entsprechend verändert werden. So wird zum Beispiel aus "Müller" das "MÜLLER". Bei Bedarf kann dies mit der Funktion `mb_strtoupper()` verhindert werden, welche mit multibyte Zeichen umgehen kann.

# Siehe auch

- [Offizielle PHP-Dokumentation zur Funktion strtoupper()](https://www.php.net/manual/de/function.strtoupper.php)
- [Offizielle PHP-Dokumentation zur Funktion ucwords()](https://www.php.net/manual/de/function.ucwords.php)
- [Offizielle PHP-Dokumentation zur Funktion mb_strtoupper()](https://www.php.net/manual/de/function.mb-strtoupper.php)