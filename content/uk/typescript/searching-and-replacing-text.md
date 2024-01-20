---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це таке та чому?

Пошук та заміна тексту - це процесс, достатньо поширений у програмуванні, який дозволяє знайти специфічний шматок тексту і замінити його новим. Це простий, але могутній інструмент, який може дуже полегшити заботи програмістів.

## Як це робити:

```TypeScript
let str = "Hello, world!";
let newStr = str.replace("world", "Ukraine");
console.log(newStr);  // Prints: "Hello, Ukraine!"
```
А ось приклад, як можна виконати замену, використовуючи регулярний вираз:

```TypeScript
let str = "Hello, world! Hello, world!";
let newStr = str.replace(/world/g, "Ukraine");
console.log(newStr);  // Виведе: "Hello, Ukraine! Hello, Ukraine!"
```

## Поглиблений погляд:

Замена тексту була важливою частиною програмування від самого початку. У TypeScript і JavaScript, її реалізація базується на методі 'replace', сформованому ECMAScript - стандартом, на якому базується JavaScript.

Альтернативою до встроєного методу 'replace' можуть стати деякі сторонні бібліотеки, такі як lodash, які надають ще більшу гнучкість при роботі з текстом.

Виконуючи пошук та замену тексту, важливо розуміти, що 'replace' замінює тільки перше входження тексту. Якщо вам потрібно замінити всі входження, використовуйте глобальний флаг 'g' з регулярним виразом, як показано в прикладі вище.

## Дивіться також:

- [MDN документация по JavaScript replace](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Handbook: Regular Expressions](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#regexps)
- [Lodash строкові методи](https://lodash.com/docs/#replacemethod)