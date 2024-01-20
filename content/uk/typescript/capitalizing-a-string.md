---
title:                "Перетворення рядка на великі літери"
html_title:           "TypeScript: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Зміна регістру рядків в TypeScript

## Що це і навіщо?
Зміна регістру рядків полягає в перетворенні перших букв кожного слова на великі (роблення їх "прописними"). Ми це робимо для форматування тексту, наприклад, для заголовків або імен в контекстах, де це стає стандартом.

## Як це робити:
Ось як можна це зробити в TypeScript:

```TypeScript
function capitalizeString(str: string): string {
   return str.replace(/\b\w/g, function (char) {
      return char.toUpperCase();
   });
}
console.log(capitalizeString("hello, world!")) // "Hello, World!"
```

Ви бачите, як маленькі букви "h" та "w" у "Hello, World!" були замінені на великі "H" та "W".

## Глибше занурення
1. Історичний контекст: Ідея приведення рядків до великого регістру зародилась задовго до появи TypeScript. Насправді, це одна з базових операцій обробки тексту в більшості мов програмування.
2. Альтернативи: В TypeScript можна використовувати метод `toLocaleUpperCase()` для адаптивного зміщення до верхнього регістру, який ураховує специфічні для місцевості варіанти літер.
3. Деталі реалізації: Наша функція `capitalizeString` використовує регулярний вираз `/ \b\w / g` для пошуку першої букви кожного слова в рядку та заміни її на прописний варіант за допомогою методу `toUpperCase()`.

## Дивіться також
2. [Javascript String toUpperCase()](https://www.w3schools.com/jsref/jsref_touppercase.asp)
3. [Mastering JavaScript Single Line String Manipulation](https://itnext.io/making-your-javascript-functions-more-readable-by-writing-less-c0a351af38e4)