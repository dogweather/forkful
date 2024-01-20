---
title:                "Перетворення рядка на великі літери"
html_title:           "Javascript: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та чому?

Зміна регістру строк в JavaScript - це процес перетворення першої літери рядка у велику (або 'капіталізація'). Програмісти роблять це, щоб покращити читабельність та дотримуватись конвенцій набору тексту.

## Як це зробити:

Це просто. Ось як ви можете капіталізувати рядок у JavaScript:

```Javascript
let str = 'hello world';
let capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(capitalizedStr);  // Outputs: 'Hello world'
```

Тут ми використовуємо `charAt(0)` для отримання першої літери, а `toUpperCase()` щоб змінити її на велику. Потім ми додаємо решту рядка, використовуючи `slice(1)`.

## В глибину

1. Історичний контекст: JavaScript не має вбудованої функції для капіталізації, тому для цього ми використовуємо комбінацію `charAt()`, `toUpperCase()`, та `slice()`.

2. Альтернативи: Іншим можливим рішенням для JavaScript буде створення прототипу:

```Javascript
String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

let str = 'hello world';
console.log(str.capitalize()); // Виводить: 'Hello world'
```

3. Деталі реалізації: Коли ви викликаєте `toUpperCase()` на рядку, він повертає новий рядок, бо JS-рядки є незмінними. 

## Див. також

- [MDN Web Docs: String.prototype.charAt()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs: String.prototype.toUpperCase()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs: String.prototype.slice()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/slice)