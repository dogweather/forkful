---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це і чому?
Складання рядків - це процес об’єднання двох або більше рядків в програмуванні. Програмісти роблять це для маніпулювання даними, створення динамічного вмісту, або формування повідомлень для користувача.

## Як зробити:
Для складання рядків у TypeScript використовуються оператори `+` або `${}` в шаблонних літералах. Ось декілька прикладів:

```TypeScript
let hello: string = "Привіт, ";
let world: string = "світ!";
let helloWorld: string = hello + world;
console.log(helloWorld); // "Привіт, світ!"

let name: string = "Валя";
console.log(`Привіт, ${name}!`); // "Привіт, Валя!"
```

## Поглиблений аналіз
Складання рядків - не нова концепція, вона присутня в більшості мов програмування. Однак, в TypeScript це можливо зробити двома способами: за допомогою оператора `+` і за допомогою шаблонних літералів `${}`. Шаблонні літерали були введені в ES6 і є більш потужними та гнучкими, дозволяючи вставляти вирази безпосередньо в рядок.

Є різні альтернативи для складання рядків, такі як методи `concat()`, `join()`, або навіть оператор `+=`. Втім, в TypeScript, оскільки він використовує стандарти ES6, шаблонні літерали є часто використовуваним методом.

## Дивіться також
- [Рядки і шаблонні літерали в TypeScript](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [Тема складання рядків в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Grammar_and_types#Склеювання_рядків)