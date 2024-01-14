---
title:    "Javascript: Die Verwendung von regulären Ausdrücken"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Regular Expressions sind ein extrem nützliches Werkzeug in der Javascript-Programmierung. Sie ermöglichen es uns, komplexe Muster in Strings zu suchen und zu manipulieren. Mit regulären Ausdrücken können wir Texte validieren, Filter anwenden und vieles mehr. Sie sparen uns viel Zeit und machen unsere Programme effizienter.

## Wie man reguläre Ausdrücke in Javascript verwendet

Um reguläre Ausdrücke in Javascript zu verwenden, müssen wir sie zuerst erstellen. Wir können dies einfach tun, indem wir den `RegExp` Konstruktor verwenden und ein Muster und eine Option als Argumente übergeben. Zum Beispiel, um eine Email-Adresse zu validieren, könnten wir folgenden regulären Ausdruck verwenden:

```Javascript
const emailRegex = new RegExp("[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,3}", "i");
```

In diesem Beispiel haben wir ein Muster erstellt, das eine Kombination aus Buchstaben, Zahlen und Sonderzeichen auf der linken Seite des @-Symbols erwartet, gefolgt von einem Punkt und einer Domain wie `.com` oder `.de` auf der rechten Seite. Die Option `i` steht für "ignore case", was bedeutet, dass die Groß- und Kleinschreibung nicht berücksichtigt wird.

Um zu überprüfen, ob ein String zu diesem Muster passt, können wir die `test()` Methode verwenden:

```Javascript
console.log(emailRegex.test("haus@gmail.com")); // Output: true
console.log(emailRegex.test("test@123456789.com")); // Output: true
console.log(emailRegex.test("123gmail.com")); // Output: false
```

Mit regulären Ausdrücken können wir auch Teile eines Strings extrahieren, indem wir sogenannte "Capturing Groups" verwenden. In unserem vorherigen Beispiel könnten wir die E-Mail-Domain extrahieren, indem wir Klammern um den Teil des Ausdrucks legen, den wir zurückgeben möchten:

```Javascript
const regex = new RegExp("[a-z0-9._%+-]+@([a-z0-9.-]+\.[a-z]{2,3})", "i");
const email = "hello@blogpost.de";

const domain = email.match(regex)[1];
console.log(domain); // Output: blogpost.de
```

Es gibt viele weitere Methoden und Möglichkeiten, reguläre Ausdrücke in Javascript zu verwenden. Es wird empfohlen, sich ausführlich darüber zu informieren, bevor man sie in Projekten einsetzt.

## Tiefergehende Einblicke in reguläre Ausdrücke

Reguläre Ausdrücke sind sehr nützlich, aber auch komplex und können schwierig zu lesen und zu verstehen sein. Ein guter Weg, um ihre Funktionsweise zu verstehen, ist es, sich mit den verschiedenen Metazeichen vertraut zu machen, die verwendet werden, um Muster zu erstellen. Einige der wichtigsten sind:

- `*` bedeutet "beliebig viele" und wird verwendet, um eine Wiederholung des vorhergehenden Zeichens oder Musters anzuzeigen
- `+` bedeutet "mindestens einmal"
- `?` bedeutet "null oder einmal"
- `.` steht für jedes Zeichen außer einem Zeilenumbruch
- `[]` werden verwendet, um eine Menge von Zeichen zu definieren, die an dieser Position im Muster erwartet werden
- `()` wird verwendet, um Capturing Groups zu erstellen

Es gibt noch viele weitere Metazeichen und Funktionen, die reguläre Ausdrücke in Javascript bieten. Es lohnt sich also, sich tiefer in dieses Thema einzuarbeiten.

## Siehe auch

- [Reguläre Ausdrücke in Javascript - MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Reguläre Ausdrücke Cheat Sheet - JavaScript.info](https://javascript.info/regular-expressions-cheatsheet)
- [RegExr - interaktiver Regulärer Ausdruck Tester](https://regexr.com/)