---
title:                "Интерполяция строки"
aliases: - /ru/javascript/interpolating-a-string.md
date:                  2024-01-28T23:58:54.835257-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Интерполяция строк — это способ встраивания переменных непосредственно в строку. Программисты используют его для эффективного соединения переменных и строк, что делает код более читабельным и удобным для поддержки.

## Как это делается:

В JavaScript интерполяция строк часто выполняется с использованием литералов шаблонов. Вот как вы можете это сделать:

```javascript
const name = 'Alice';
const message = `Привет, ${name}! Как твои дела сегодня?`;
console.log(message); // Выводит: Привет, Alice! Как твои дела сегодня?
```

Вы также можете выполнять операции внутри заполнителей:

```javascript
const a = 10;
const b = 5;
console.log(`Десять умножить на пять равно ${a * b}.`); // Выводит: Десять умножить на пять равно 50.
```

## Глубокое погружение

Исторически, интерполяция строк в JavaScript была не такой простой. До ES6 (ECMAScript 2015), конкатенация обычно выполнялась с использованием оператора `+`:

```javascript
var name = 'Bob';
var message = 'Привет, ' + name + '! Как твои дела сегодня?';
```

С введением ES6 появились литералы шаблонов (между обратными кавычками \` \`), предложив более простой синтаксис с заполнителями `${}`.

Альтернативы интерполяции строк включают конкатенацию строк с помощью оператора `+` и метода `concat()`, или использование функций в стиле `sprintf` из сторонних библиотек.

Производительность литералов шаблонов в целом сравнима с этими более старыми методами. Однако читабельность и возможность включения выражений (как `${a * b}`) в строки делают литералы шаблонов сильным выбором для разработчиков.

## Смотрите также

- MDN о литералах шаблонов: https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Template_literals
- Конкатенация строк в JavaScript: https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Operators/String_Operators
- История модуля JavaScript "ECMAScript": https://www.ecma-international.org/publications-and-standards/standards/ecma-262/
