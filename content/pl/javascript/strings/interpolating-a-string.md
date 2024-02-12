---
title:                "Interpolacja łańcuchów znaków"
aliases:
- /pl/javascript/interpolating-a-string.md
date:                  2024-01-20T17:51:04.776000-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
W interpolacji stringów chodzi o wstawianie wartości zmiennych w środek łańcucha tekstowego. Robimy to, by łatwiej budować dynamiczne wiadomości i szablony.

## How to:
**W JavaScript można interpolować stringi przy użyciu template literals, oto jak:**

```javascript
const name = 'Jan';
const message = `Cześć, ${name}! Jak się masz?`;
console.log(message); // "Cześć, Jan! Jak się masz?"
```

**Potrzebujesz wyrażenia w środku? Żaden problem:**

```javascript
const x = 5;
const y = 10;
console.log(`Suma ${x} i ${y} to ${x + y}.`); // "Suma 5 i 10 to 15."
```

**Interpolacja działa też z funkcjami:**

```javascript
function formatCurrency(amount) {
  return `${amount.toFixed(2)} zł`;
}

const price = 29.99;
const message = `Do zapłaty: ${formatCurrency(price)}`;
console.log(message); // "Do zapłaty: 29.99 zł"
```

## Deep Dive
Interpolacja stringów istnieje w JavaScript od wprowadzenia ES6 w 2015 roku. Wcześniej używaliśmy konkatenacji, czyli łączenia stringów przy użyciu `+`:

```javascript
var name = 'Jan';
var message = 'Cześć, ' + name + '! Jak się masz?';
```

Jednakże, template literals (oznaczone przez backticks `` ` ``) uczyniły ten proces bardziej eleganckim i czytelnym.

Ciekawostką jest, że niektóre języki mają interpolację stringów już od dłuższego czasu, przykładowo w Ruby czy Perl.

Alternatywą interpolacji w JavaScript może być również stosowanie funkcji `replace` lub bibliotek zewnętrznych jak lodash, które oferują funkcję `_.template`.

```javascript
const data = { name: "Jan" };
const compiled = _.template("Cześć, <%= name %>!");
console.log(compiled(data)); // "Cześć, Jan!"
```

Pod względem wydajności, interpolacja stringów może mieć lekką przewagę nad konkatenacją, ponieważ JavaScript silniki zoptymalizowane są pod kątem pracy z template literals.

## See Also
- MDN Web Docs: Template literals (Template strings) - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- You Don't Need Lodash/Underscore - https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore
