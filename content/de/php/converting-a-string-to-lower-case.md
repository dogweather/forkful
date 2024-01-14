---
title:                "PHP: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wer sich mit der Programmiersprache PHP beschäftigt, wird früher oder später auf die Aufgabe stoßen, einen String in Kleinbuchstaben umzuwandeln. Dies kann aus verschiedenen Gründen notwendig sein, beispielsweise um die Eingabe eines Benutzers im gleichen Format zu erhalten oder um eine einheitliche Formatierung von Daten zu gewährleisten.

## Wie

Die Umwandlung eines Strings in Kleinbuchstaben ist in PHP sehr einfach und kann mit der Funktion `strtolower()` durchgeführt werden. Diese Funktion nimmt als Argument den zu konvertierenden String und gibt anschließend den neuen, in Kleinbuchstaben geschriebenen String zurück. Im Folgenden sind einige Beispiele für die Verwendung von `strtolower()` aufgeführt:

```PHP
$input = "Hallo, WELT!";

echo strtolower($input); // gibt "hallo, welt!" aus
```

```PHP
$input = "DU Und ICH";

echo strtolower($input); // gibt "du und ich" aus
```

Wie in den obigen Beispielen gezeigt, werden alle Buchstaben in der originalen Groß-/Kleinschreibung in Kleinschreibung umgewandelt. Es ist auch möglich, nur den ersten Buchstaben des Strings in einen Kleinbuchstaben zu verwandeln und den Rest unverändert zu lassen. Dies kann mit der Funktion `lcfirst()` erreicht werden, die ähnlich wie `strtolower()` funktioniert, jedoch nur den ersten Buchstaben in Kleinschreibung umwandelt.

## Deep Dive

Der Grund, warum die Funktionen `strtolower()` und `lcfirst()` so effektiv und einfach zu verwenden sind, liegt in ihrem Umgang mit Sonderzeichen und Umlauten. PHP ist eine Unicode-fähige Sprache und kann daher mit einer Vielzahl von Zeichen und Zeichensätzen umgehen. Die Funktionen `strtolower()` und `lcfirst()` sind so konzipiert, dass sie die Sprach- und Zeichensatzspezifikationen des Betriebssystems und der verwendeten Zeichenkodierung berücksichtigen. Dies bedeutet, dass sie nicht nur mit den Buchstaben des englischen Alphabets funktionieren, sondern auch mit Buchstaben aus anderen Sprachen, wie beispielsweise deutschen Umlauten (`ä`, `ö`, `ü`), französischen Akzenten (`é`, `è`, `ê`) oder sogar kyrillischen Buchstaben.

Es ist jedoch wichtig zu beachten, dass die Art und Weise, wie Buchstaben in Kleinbuchstaben umgewandelt werden, von der verwendeten Programmiersprache und den spezifischen Sprach- und Zeichensatzkodierungen abhängt. Es kann daher zu Unterschieden kommen, wenn dieselbe Aufgabe in anderen Programmiersprachen ausgeführt wird.

## Siehe auch

- [PHP Dokumentation zu strtolower()](https://www.php.net/manual/de/function.strtolower.php)
- [PHP Dokumentation zu lcfirst()](https://www.php.net/manual/de/function.lcfirst.php)
- [Unicode Consortium](https://unicode.org/)
- [Unicode-Zeichenkodierung in PHP](https://www.php.net/manual/de/book.unicode.php)