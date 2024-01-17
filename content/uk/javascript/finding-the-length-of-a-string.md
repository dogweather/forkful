---
title:                "Знаходження довжини рядка"
html_title:           "Javascript: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Отримання довжини рядка є важливою задачею для програмістів у Javascript. Це дає можливість оперувати з даними та виконувати різноманітні дії з текстом.

## Як виконати:
```Javascript
// створюємо змінну з рядком
let str = "Україна";

// використовуємо властивість .length, яка повертає довжину рядка
console.log(str.length); // виводиться 7
```

## Глибше занурення:
Отримання довжини рядка відіграє важливу роль в програмуванні. Строкові значення є одними з основних типів даних у Javascript, тому вміння працювати з ними є необхідним для будь-якого програміста. В минулому, для підрахунку довжини рядка використовувалося власне поле у об'єктах, але з появою властивості .length цей процес став більш простим і зручним. Як альтернативу, можна використовувати функцію .toString(), яка також дає змогу отримати довжину рядка.

## Подивіться також:
- [Документація MDN про властивість .length](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Документація MDN про функцію .toString()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toString)