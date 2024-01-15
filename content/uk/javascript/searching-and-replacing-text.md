---
title:                "Пошук та заміна тексту."
html_title:           "Javascript: Пошук та заміна тексту."
simple_title:         "Пошук та заміна тексту."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Часто, коли ми пишемо код, нам потрібно замінити конкретний текст на інший текст. Наприклад, якщо ми пишемо програму для перекладу тексту, нам може знадобитися замінити англійські слова на українські. У таких випадках, пошук та заміна тексту є важливими інструментами для виконання цієї задачі.

## Як зробити пошук та заміну тексту в Javascript

Щоб виконати пошук та заміну тексту в Javascript, нам потрібно використовувати метод `replace()` для рядків. Давайте розглянемо приклад коду, який замінює слово "hello" на "привіт":

```Javascript
var str = "hello world";
var newStr = str.replace("hello", "привіт");
console.log(newStr); // виведе "привіт world"
```

Метод `replace()` приймає два аргументи: перший - це текст, який ми шукаємо, а другий - це текст, на який ми замінюємо. Якщо ми хочемо замінити всі входження певного слова, ми можемо використовувати регулярні вирази:

```Javascript
var str = "hello hello hello";
var newStr = str.replace(/hello/g, "привіт");
console.log(newStr); // виведе "привіт привіт привіт"
```

Можна також використовувати функцію заміни, яка дозволяє нам виконувати додаткові дії з кожним входженням тексту:

```Javascript
var str = "Hello World";
var newStr = str.replace(/hello/g, function(match) {
  return match.toUpperCase();
});
console.log(newStr); // виведе "HELLO World"
```

## Глибокий аналіз

Метод `replace()` може приймати функцію заміни, яка дозволяє нам виконувати складні операції з текстом, наприклад, використовувати регулярні вирази для заміни певних частин тексту. Також, метод може бути використаний для заміни символів в рядках з використанням UTF-8 кодування, що дозволяє робити пошук і заміну тексту в різних мовах.

## Дивіться також
- [Документація по методу replace() в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Приклади використання методу replace()](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Регулярні вирази в Javascript](https://www.w3schools.com/js/js_regexp.asp)