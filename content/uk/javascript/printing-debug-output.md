---
title:                "Виведення відлагоджувального виводу"
html_title:           "Javascript: Виведення відлагоджувального виводу"
simple_title:         "Виведення відлагоджувального виводу"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому
Велика частина процесу програмування полягає в розв'язанні проблем та виявленні помилок. Одним з корисних інструментів, який допомагає у цьому завданні, є виведення відладочних повідомлень. Він дозволяє вам слідкувати за даними та перевіряти перебіг вашої програми, що значно спрощує процес знаходження та виправлення помилок.

## Як
Використовуючи функцію `console.log()`, ви можете виводити різні значення для подальшого аналізу. Наприклад:

```Javascript
let name = "John";
let age = 25;
let isMarried = true;

console.log("Name:", name);
console.log("Age:", age);
console.log("Married:", isMarried);
```

Виходячи з цього коду, в консолі з'являться наступні повідомлення:

```
Name: John
Age: 25
Married: true
```

Ви можете використовувати це для виведення будь-яких змінних чи результатів функцій для подальшої перевірки та діагностики.

## Глибоке заглибкування
Крім функції `console.log()`, ви також можете використовувати інші методи, такі як `console.error()` або `console.warn()`, для виведення повідомлень про помилки або попередження надзвичайних ситуацій, відповідно.

Також варто зазначити, що ви можете використовувати `%s` для вставки значень у рядок, або `%c` для задання стилю тексту. Наприклад:

```Javascript
let time = "10:00 AM";
console.log("The time is %s", time); // виведе 'The time is 10:00 AM'

console.log("%c This text is red", "color: red"); // виведе 'This text is red' з червоним кольором
```

## Дивіться також
- [Використання консолі браузера на MDN](https://developer.mozilla.org/uk/docs/Tools/Web_Console/Introduction)
- [Про відладку в JavaScript на W3Schools](https://www.w3schools.com/js/js_debugging.asp)
- [Відладка з використанням DevTools в Chrome на Google Developers](https://developers.google.com/web/tools/chrome-devtools/javascript)