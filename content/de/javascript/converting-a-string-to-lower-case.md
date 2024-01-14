---
title:                "Javascript: Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren kann es manchmal notwendig sein, dass man einen Teil oder auch den gesamten Text in Kleinbuchstaben umwandeln muss. Dies kann verschiedene Gründe haben, wie zum Beispiel die Vergleichbarkeit von Strings oder die Einheitlichkeit der Groß- und Kleinschreibung in einer Anwendung. In diesem Artikel werden wir uns genauer anschauen, wie man in Javascript eine Zeichenkette in Kleinbuchstaben umwandelt und welche Funktionen dafür zur Verfügung stehen.

# Wie geht das?

Um eine Zeichenkette in Javascript in Kleinbuchstaben umzuwandeln, gibt es verschiedene Methoden, die zur Verfügung stehen. Eine einfache Möglichkeit ist die Verwendung der `toLowerCase()` Methode. Diese wandelt alle Buchstaben in der Zeichenkette in Kleinbuchstaben um und gibt diese als neuen String zurück. Hier ein Beispielcode:

```Javascript
var text = "Hallo, wie geht es dir?";
var lowerCaseText = text.toLowerCase();
console.log(lowerCaseText);
```

Das Ergebnis in der Konsole wäre dann: "hallo, wie geht es dir?". Wie man sieht, wurden alle Buchstaben klein geschrieben.

Eine weitere Möglichkeit ist die Verwendung der `toLocaleLowerCase()` Methode. Diese unterscheidet sich von `toLowerCase()` in der Art, dass sie auch Zeichen aus anderen Sprachen in Kleinbuchstaben umwandelt, je nachdem in welcher Sprache die Anwendung läuft. Hier ein Beispielcode:

```Javascript
var text = "ÍSLAND";
var lowerCaseText = text.toLocaleLowerCase();
console.log(lowerCaseText);
```

Das Ergebnis hier wäre dann: "ísland", da in diesem Beispiel die Sprache des Browsers englisch ist. Wenn die Sprache auf beispielsweise Isländisch eingestellt wäre, würde das Ergebnis "island" lauten.

# Tiefer gehende Informationen

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, verwendet Javascript das ASCII Zeichenschema. Dies bedeutet, dass jeder Buchstabe in eine Zahl umgewandelt wird und diese Zahl dann entsprechend in Kleinbuchstaben übersetzt wird. Großbuchstaben haben dabei einen höheren Zahlenwert als Kleinbuchstaben, daher kann man sich merken, dass bei der Umwandlung in Kleinbuchstaben immer ein bestimmter Zahlenwert abgezogen wird.

Es ist außerdem wichtig zu beachten, dass die `toLowerCase()` und `toLocaleLowerCase()` Methoden keine Auswirkungen auf Sonderzeichen wie beispielsweise Akzente haben. In diesen Fällen muss man auf andere Methoden zurückgreifen, wie zum Beispiel der `normalize()` Methode in Kombination mit `toLowerCase()`, um auch diese Zeichen in Kleinbuchstaben umzuwandeln.

# Siehe auch

- [String.prototype.toLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [ASCII Zeichenschema](https://www.asciitable.com/)

Vielen Dank fürs Lesen und happy coding!