---
title:                "Unterstrings extrahieren"
html_title:           "PHP: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

Was ist substring?

Ein substring ist ein Teil einer längeren Zeichenkette. Es ist eine nützliche Funktion in der Programmierung, die es ermöglicht, bestimmte Teile von Text zu extrahieren und sie für verschiedene Zwecke zu nutzen.

Warum nutzen Programmierer substrings?
Programmierer verwenden substrings aus verschiedenen Gründen. Zum Beispiel kann es nützlich sein, um Daten aus einer langen Zeichenkette zu extrahieren, um sie in einer anderen Funktion weiterzuverarbeiten. Es kann auch zur Validierung von Benutzereingaben verwendet werden oder um bestimmte Informationen aus einem Text zu extrahieren.

Wie man substrings in PHP extrahiert:
Das Extrahieren von substrings ist in PHP sehr einfach und erfordert nur eine Zeile Code. Hier ist ein Beispiel, wie man das Wort "Welt" aus dem Satz "Hallo Welt!" extrahiert:

```
<?php
$text = "Hallo Welt!";
$sub = substr($text, 6, 4);
echo $sub; // Ausgabe: Welt
?>
```

Um einen substring in PHP zu extrahieren, muss die Funktion `substr()` verwendet werden. Die ersten beiden Argumente sind die Zeichenkette, aus der der substring extrahiert werden soll, und die Startposition, an der der substring beginnt. Das optionale dritte Argument ist die Länge des substrings, die standardmäßig auf die Länge des restlichen Textes gesetzt ist.

Tiefergehende Informationen:
Die Funktion `substr()` existiert bereits seit den frühen Versionen von PHP und wurde auch in anderen Sprachen wie C und Java implementiert. Es gibt auch alternative Funktionen wie `mb_substr()`, die für den Umgang mit multibyte-Zeichen implementiert wurden.

Ein weiterer wichtiger Aspekt beim Extrahieren von substrings ist die Behandlung von Unicode-Zeichen, die aus mehreren Bytes bestehen können. In PHP muss dafür die mbstring-Erweiterung aktiviert sein, um eine genaue Bearbeitung solcher Zeichen zu gewährleisten.

Weitere Ressourcen:
- Offizielle Dokumentation zu `substr()`: https://www.php.net/manual/de/function.substr.php
- Weitere Informationen zu mb_substr(): https://www.php.net/manual/de/function.mb-substr.php