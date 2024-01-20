---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Видалення символів, що відповідають певному шаблону - це процес відфільтрування рядків, залишаючи лише ті символи, які не відповідають заданому шаблону. Програмісти це роблять для очищення даних, форматування тексту та забезпечення коректної обробки вводу.

## Як це виконати:

Щоб виділити символи, що відповідають певному шаблону в TypeScript, ви можете використати метод `replace()` із регулярними виразами. Давайте попрацюємо над прикладом:

```TypeScript
let text: string = "Цей приклад видалить усi цифри 123 і символи #$@";
let result: string = text.replace(/[0-9A-Za-z#$@]/g, "");
console.log(result);
```
Цей код виведе: "Цей приклад видалить усі цифри і символи ".

## Занурення в деталі

Видалення символів, що відповідають певному шаблону, було неодмінною частиною мови програмування ще від часів Perl. У сучасному TypeScript використовуються регулярні вирази для вирішення цієї задачі, але є й інші альтернативи. Наприклад, можна використовувати метод `split()`, щоб розділити рядок на масив символів, потім застосувати метод `filter()` для відфільтрування непотрібних символів і нарешті конвертувати результат у рядок за допомогою методу `join()`. Цей підхід, хоч і більш об'ємний, може бути більш зрозумілим для новачків.

## Дивись також

1. [Регулярні вирази в JavaScript і TypeScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)
2. [Метод replace() в JavaScript і TypeScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
3. [Методи масиву в JavaScript і TypeScript: split(), filter(), join()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Array)