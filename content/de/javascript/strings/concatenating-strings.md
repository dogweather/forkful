---
date: 2024-01-20 17:35:06.461991-07:00
description: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren\
  \ Textst\xFCcken (Strings) zu einem. Wir nutzen sie, um dynamischen Text zu erstellen\
  \ oder\u2026"
lastmod: '2024-03-11T00:14:28.162081-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren Textst\xFC\
  cken (Strings) zu einem. Wir nutzen sie, um dynamischen Text zu erstellen oder\u2026"
title: "Zeichenketten verkn\xFCpfen"
---

{{< edit_this_page >}}

## Was & Warum?

String-Konkatenation ist das Zusammenfügen von zwei oder mehreren Textstücken (Strings) zu einem. Wir nutzen sie, um dynamischen Text zu erstellen oder Informationen anzuzeigen, die sich während des Programmablaufs ändern können.

## So geht's:

```javascript
// Mit dem Plus-Operator (+)
let gruss = "Hallo, " + "Welt!";
console.log(gruss); // "Hallo, Welt!"

// Mit Template Literals (ab ES6)
let planet = "Welt";
let begruessung = `Hallo, ${planet}!`;
console.log(begruessung); // "Hallo, Welt!"
```

## Deep Dive

Historisch wurde die String-Konkatenation hauptsächlich mit dem Plus-Operator durchgeführt. Seit ECMAScript 6 (ES6) gibt es die eleganteren Template Literals (Template Strings), die das Einbetten von Variablen und Ausdrücken in Strings erheblich vereinfachen.

Alternativen zur Konkatenation:

- `concat()`-Methode: `str1.concat(str2)`, weniger gebräuchlich wegen umständlicher Syntax.
- Array-Joining: `['Hallo, ', 'Welt!'].join('')`, praktisch bei vielen String-Stücken.

Implementierungsdetails:

- Performanz: Viele kleine Konkatenationen können performanter sein als wenige große.
- Speichermanagement: Modernes JS optimiert die Speichernutzung bei der String-Konkatenation automatisch.

## Siehe auch

- [MDN Web Docs: Template Literals](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Template_literals)
- [MDN Web Docs: String.prototype.concat()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [You Don't Know JS: Es6 & Beyond](https://github.com/getify/You-Dont-Know-JS/) - Buch über moderne JavaScript-Features, einschließlich Template Literals.
