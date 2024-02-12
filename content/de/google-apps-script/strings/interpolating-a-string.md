---
title:                "Interpolation einer Zeichenkette"
aliases:
- /de/google-apps-script/interpolating-a-string/
date:                  2024-02-01T21:55:20.718901-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpolation einer Zeichenkette"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/interpolating-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Zeichenketteninterpolation in Google Apps Script ermöglicht das dynamische Einbetten von Ausdrücken in Zeichenketten und erleichtert die Erstellung von lesbarem und wartbarem Code. Programmierer verwenden diese Technik, um Variablen und Ausdrücke nahtlos in Zeichenketten einzufügen, ohne die umständliche Syntax für die Konkatenation zu verwenden.

## Wie:

In Google Apps Script wird die Zeichenketteninterpolation durch Template-Literale erreicht. Dabei handelt es sich um Zeichenkettenliterale, die eingebettete Ausdrücke erlauben und durch Rückstriche (\`) anstatt der üblichen Anführungszeichen gekennzeichnet sind. So können Sie sie verwenden:

```javascript
// Ein grundlegendes Beispiel
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Hallo, ${user}!`); // Ausgabe: Hallo, Alice!
}

// Verwendung von Ausdrücken
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Fünf plus zehn ist ${a + b}.`); // Ausgabe: Fünf plus zehn ist 15.
}

// Mehrzeilige Zeichenketten
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Dies ist eine mehrzeilige Zeichenkette:
Hallo zusammen,
wir besprechen heute ${item}.`);
  // Ausgabe:
  // Dies ist eine mehrzeilige Zeichenkette:
  // Hallo zusammen,
  // wir besprechen heute Google Apps Script.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Diese Beispiele veranschaulichen die grundlegende Verwendung, das Einbetten von Ausdrücken und das Erstellen von mehrzeiligen Zeichenketten mit interpolierten Werten.

## Vertiefung

Template-Literale, einschließlich der Zeichenketteninterpolation, wurden in ECMAScript 2015 (ES6) eingeführt und anschließend in Google Apps Script übernommen. Davor mussten sich Programmierer ausschließlich auf die Zeichenkettenkonkatenation verlassen, was bei komplexen Zeichenketten oder bei der Integration vieler Variablenwerte umständlich werden konnte.

```javascript
// Alte Methode (vor ES6)
var user = 'Bob';
console.log('Hallo, ' + user + '!');
```

Obwohl die Zeichenketteninterpolation eine leistungsstarke Funktion ist, ist es wichtig, auf die Kontexte zu achten, in denen sie verwendet wird. Beispielsweise kann das direkte Einbetten von Benutzereingaben ohne angemessene Sanitierung zu Sicherheitsproblemen führen, wie etwa Injektionsangriffen. Entwickler von Google Apps Script sollten sicherstellen, dass alle dynamischen Inhalte, die in Zeichenketten interpoliert werden, ordnungsgemäß überprüft oder bereinigt werden.

Im Vergleich zu anderen Programmiersprachen existiert das Konzept der Zeichenketteninterpolation weit verbreitet, mit unterschiedlicher Syntax. Python verwendet f-Strings oder die `format`-Methode, Ruby verwendet `#{}` innerhalb von doppelten Anführungszeichen, und viele moderne Sprachen haben ähnliche Funktionen aufgrund der Lesbarkeit und Bequemlichkeit, die sie bieten, übernommen.

Obwohl Google Apps Script keine zusätzlichen Interpolationsfunktionen bietet, die über die von ECMAScript-Standards hinausgehen, ist die vorhandene Funktionalität leistungsfähig und ausreichend für die meisten Anwendungsfälle. Entwickler, die aus Sprachen mit aufwendigeren Interpolationsmechanismen kommen, müssen möglicherweise ihre Erwartungen anpassen, werden aber wahrscheinlich die Einfachheit und Effizienz von Template-Literalen in Google Apps Script zu schätzen wissen.
