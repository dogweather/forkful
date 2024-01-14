---
title:    "PHP: Verwendung von regulären Ausdrücken"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Warum

Das Verwenden von regulären Ausdrücken ist eine effiziente Möglichkeit, Muster in Strings zu finden und zu manipulieren. Es ist vor allem nützlich für Programmierer, die oft mit großen Mengen von Daten arbeiten und bestimmte Muster in diesen Daten finden müssen.

##Wie geht es?

Um reguläre Ausdrücke in PHP zu verwenden, müssen Sie die integrierten Funktionen `preg_match()` oder `preg_replace()` verwenden. Diese Funktionen akzeptieren als Parameter sowohl den regulären Ausdruck als auch den zu durchsuchenden String.

Ein Beispiel für die Verwendung von `preg_match()`:

```PHP
$pattern = '/[0-9]{3}-[0-9]{2}-[0-9]{4}/'; // sucht nach US-amerikanischen Sozialversicherungsnummern
$string = "Meine Sozialversicherungsnummer ist 123-45-6789.";
if (preg_match($pattern, $string)) {
  echo "Eine gültige Sozialversicherungsnummer wurde gefunden.";
} else {
  echo "Es wurde keine gültige Sozialversicherungsnummer gefunden.";
}
```

Die Ausgabe wäre:

```
Eine gültige Sozialversicherungsnummer wurde gefunden.
```

Ein weiteres Beispiel für die Verwendung von `preg_replace()`:

```PHP
$pattern = '/[aeiou]/'; // ersetzt alle Vokale durch den Buchstaben "x"
$string = "Wie geht es dir?";
$new_string = preg_replace($pattern, "x", $string);
echo $new_string;
```

Die Ausgabe wäre:

```
Wx gxht xs dxr?
```

##Deep Dive

Reguläre Ausdrücke folgen bestimmten Syntaxregeln und können sehr komplex werden. Hier sind einige wichtige Konzepte, die Sie beim Schreiben von regulären Ausdrücken beachten sollten:

- Verwenden Sie Zeichenklassen, um bestimmte Arten von Zeichen zu finden (z. B. Buchstaben, Zahlen, Leerzeichen).
- Verwenden Sie Quantifizierer, um anzugeben, wie oft ein bestimmtes Zeichen oder eine Gruppe von Zeichen erscheinen soll (z. B. `{3}` für genau 3 Vorkommen).
- Verwenden Sie spezielle Zeichen wie `?` (0 oder 1 Vorkommen) und `+` (1 oder mehr Vorkommen) für weitere Flexibilität.
- Verwenden Sie Gruppierungsklammern `()` um Teilausdrücke zu definieren, die später referenziert werden können.
- Verwenden Sie Anfangs- und Endanker `^` und `$` für genauere Übereinstimmungen am Anfang und Ende eines Strings.

Eine umfassende Liste aller verfügbaren Befehle und Regeln für reguläre Ausdrücke in PHP finden Sie in der [offiziellen Dokumentation](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php).

##Siehe auch

- [Einführung in reguläre Ausdrücke in PHP](https://www.php.net/manual/en/book.pcre.php)
- [Reguläre Ausdrücke Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [20 Beispiele für die Verwendung von regulären Ausdrücken in PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)