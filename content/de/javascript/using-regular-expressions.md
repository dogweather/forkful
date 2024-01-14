---
title:                "Javascript: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

"
## Warum

Warum sollte man sich mit regulären Ausdrücken (regular expressions) beschäftigen? Nun, diese kleinen Zeichenfolgen können eine große Hilfe bei der Verarbeitung von Texten in der Programmierung sein. Sie ermöglichen es, bestimmte Muster in Zeichenketten zu erkennen und zu manipulieren. Mit regulären Ausdrücken können wir also zum Beispiel E-Mails, Telefonnummern oder Passwörter validieren, ein Textdokument durchsuchen oder zählen, wie oft ein Wort vorkommt.

## Wie geht das?

Um reguläre Ausdrücke in JavaScript zu verwenden, müssen wir zunächst ein RegExp-Objekt erstellen. Hier ein Beispiel:

```Javascript
// Erstelle ein RegExp-Objekt, um nach einer Telefonnummer zu suchen
let telefonnummer = new RegExp('^[0-9]{3}-[0-9]{3}-[0-9]{4}$');

// Eine Zeichenkette, die überprüft werden soll
let eingabe = '555-123-4567';

// Verwende die test()-Methode, um zu überprüfen, ob die Telefonnummer in der Zeichenkette enthalten ist
if(telefonnummer.test(eingabe)) {
  console.log('Gültige Telefonnummer!');
} else {
  console.log('Keine gültige Telefonnummer.');
}
```
Das obige Beispiel erstellt ein RegExp-Objekt, das nach dem Muster von 555-123-4567 sucht. Mit der `test()`-Methode prüfen wir dann, ob unsere Eingabe diesem Muster entspricht.

Reguläre Ausdrücke bieten auch viele Möglichkeiten für die sogenannte "Suchen und Ersetzen"-Funktion. Mit der `replace()`-Methode können wir zum Beispiel bestimmte Zeichenketten innerhalb einer größeren Zeichenkette ersetzen. Hier ein Beispiel:

```Javascript
// Eine Zeichenkette mit verschiedenen Schreibweisen von "Katze"
let text = 'Die Katze kämpfte mit der Kate auf der kanarischen Insel.';

// Wir wollen alle Vorkommen von "Katze" durch "Hund" ersetzen
let neuer_text = text.replace(/Katze/g, 'Hund');

console.log(neuer_text);
// Ausgabe: Die Hund kämpfte mit der Hund auf der kanarischen Insel.
```

Beachte die Verwendung der Flag `g` neben dem regulären Ausdruck. Diese zeigt an, dass alle Vorkommen innerhalb der Zeichenkette ersetzt werden sollen.

## Sich tiefer damit beschäftigen

Reguläre Ausdrücke können zunächst etwas kompliziert erscheinen, aber sie werden mit der Zeit immer verständlicher. Außerdem gibt es viele hilfreiche Online-Tools, mit denen man reguläre Ausdrücke testen und üben kann. Eine gute Möglichkeit, um mehr über reguläre Ausdrücke zu erfahren, ist die offizielle Dokumentation von JavaScript oder Tutorials und Übungen auf Plattformen wie Codecademy oder FreeCodeCamp.

## Siehe auch

- [Offizielle JavaScript-Dokumentation zu regulären Ausdrücken](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [RegularExpression.info - Nützliche Infos und Übungen zu regulären Ausdrücken](https://www.regular-expressions.info/)
- [Codecademy - Reguläre Ausdrücke lernen](https://www.codecademy.com/learn/learn-regular-expressions)
- [FreeCodeCamp - Tutorial zu regulären Ausdrücken](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/regular-expressions/)