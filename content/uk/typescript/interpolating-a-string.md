---
title:                "Інтерполяція рядка"
html_title:           "TypeScript: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

Що & З чого?

Замінювання рядка (англ. string interpolation) - це спосіб вставляти значення змінних у рядок, щоб він був динамічним. Це дозволяє програмістам створювати більш зручні та зрозумілі рядки коду.

Як:
```TypeScript
// Приклад замінювання рядка з використанням звичайних змінних
let name = 'John'; 
console.log(`Привіт, моє ім'я ${name}`); 
// Виведе: Привіт, моє ім'я John
```
```TypeScript
// Приклад замінювання рядка з використанням виразів
let x = 5;
let y = 10;
console.log(`Сума x та y дорівнює ${x + y}`); 
// Виведе: Сума x та y дорівнює 15
```

Глибока недослідження:

Замінювання рядка було запроваджено в ECMAScript 6 (також відомому як ES2015) як альтернатива до конкатенації рядків. Це дозволяє програмістам створювати більш ефективний та зрозумілий код. Іншими способами замінювання рядка є шаблонні рядки ES5 (без використання ` замість "). У TypeScript замінювання рядка використовується для покращення читабельності та ефективності коду, а також для підтримки статичного типу даних.

Подивіться також:

- Посилання на документацію TypeScript про замінювання рядка: https://www.typescriptlang.org/docs/handbook/strings.html#template-strings
- Інформація про шаблонні рядки у JavaScript: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/template_strings