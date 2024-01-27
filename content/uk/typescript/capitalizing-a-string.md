---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?

Великі літери (capitalization) в струнах — це коли ми перетворюємо першу букву слова в велику літеру для вдосконалення читабельності або відповідності стилю. Програмісти роблять це, щоб текст виглядав охайніше чи слідував певним мовним правилам.

## Як саме:

```TypeScript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello')); // 'Hello'
console.log(capitalize('тест')); // 'Тест'
```

## Поглиблений Розбір:

Історично, великі літери використовували для підкреслення важливості слова або позначення початку речення. В програмуванні нам часто потрібно стандартизувати текстові дані для відображення користувачам або для усунення розбіжностей при порівнянні строк.

У TypeScript, як і в JavaScript, немає вбудованої функції для перетворення всієї строки в "title case" (де кожне слово починається з великої літери), тому ми часто пишемо свої власні помічники. Прогляньте код наведений вище, щоб побачити базову реалізацію capitalization для одного слова.

Як альтернативи, можна користуватися бібліотеками, такими як Lodash, що пропонують гнучкі функції для обробки строк. Однак іноді пошук простого рішення самостійно є швидшим і навчає кращому розумінню мови.

## Також Дивіться:

- MDN Web Docs по [toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase) та [toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Lodash](https://lodash.com/) – особливо функції [_.capitalize](https://lodash.com/docs/#capitalize) та [_.startCase](https://lodash.com/docs/#startCase)
- TypeScript Handbook з оглядом на [типи строк](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
