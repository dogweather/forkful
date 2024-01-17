---
title:                "Zusammenfügen von Zeichenfolgen"
html_title:           "PHP: Zusammenfügen von Zeichenfolgen"
simple_title:         "Zusammenfügen von Zeichenfolgen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/concatenating-strings.md"
---

{{< edit_this_page >}}

# Was & Warum?
String-Konkatenation ist ein Begriff, der in der Programmierung verwendet wird, um zwei oder mehr Strings zusammenzufügen. Dies erlaubt es Programmierern, dynamische Daten zu erstellen, indem sie vorhandene Strings kombinieren. Zum Beispiel könnte ein Entwickler den Text "Hallo" mit einer Variablen, die den Namen des Benutzers enthält, verketten, um eine personalisierte Begrüßung zu erstellen. Diese Technik ist besonders nützlich in der Webentwicklung, um dynamische Inhalte auf Webseiten zu erzeugen.

# Wie geht's:
```PHP 
echo "Hallo" . $name;
```
Ausgabe: Hallo Benutzer

In diesem Beispiel wird der "." Operator verwendet, um den String "Hallo" mit der Variablen $name zu verketten. Beachte, dass zwischen dem Punkt und der Variable kein Leerzeichen vorhanden sein darf, da dies zu einem Syntaxfehler führen würde.

# Tiefentauchen:
Die Idee der String-Konkatenation ist nicht neu und wird in vielen Programmiersprachen verwendet, einschließlich PHP. In älteren Programmiersprachen wie C wurde die Funktion strcat () verwendet, um Strings zusammenzufügen. Allerdings kann diese Methode unsicher sein, da sie nicht prüft, ob genügend Speicherplatz für den neuen String vorhanden ist. In PHP gibt es auch alternative Wege, um Strings zu verketten, wie z.B. die Funktion sprintf (), die es erlaubt, variablen Wert in einen String einzufügen.

# Siehe Auch:
- [PHP-Handbuch - String-Konkatenation](https://www.php.net/manual/de/language.operators.string.php)
- [PHP-Handbuch - sprintf() Funktion](https://www.php.net/manual/de/function.sprintf.php)
- [C++-Referenz - strcat() Funktion](https://www.cplusplus.com/reference/cstring/strcat/)