---
title:                "Перетворення рядка у нижній регістр"
html_title:           "TypeScript: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що та Чому?
Перетворення рядка в нижній регістр є процесом зміни всіх літер у рядку на їх нижній регістр, що означає, що усі великі літери будуть замінені на відповідні малі літери. Програмісти зазвичай роблять це для того, щоб легше порівнювати рядки і виконувати операції пошуку та фільтрації на рядках.

## Як це зробити:
```TypeScript
let str = "HELLO WORLD";
console.log(str.toLowerCase());
// output: "hello world"
```

```TypeScript
let str = "Привіт Світ";
console.log(str.toLowerCase());
// output: "привіт світ"
```

## Вдосконалюємось:
Перетворення рядка в нижній регістр було популярним у більш старій версії JavaScript за допомогою методу ```toLowerCase()``` або за допомогою регулярних виразів. Однак у TypeScript можна використовувати нові функції, такі як ```toLowerCase()``` з перечисленими параметрами, щоб перетворити рядок в нижній регістр з використанням іншої абетки або мови.

## Дивіться також:
- [MDN Web Docs](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) - документація MDN про метод ```toLowerCase()```.