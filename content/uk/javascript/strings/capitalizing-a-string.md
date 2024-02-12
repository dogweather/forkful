---
title:                "Зробити першу літеру рядка великою"
aliases:
- /uk/javascript/capitalizing-a-string.md
date:                  2024-02-03T19:06:09.333825-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Перетворення рядка з великої літери означає перетворення першого символу рядка на велику літеру, залишаючи решту символів без змін. Ця операція часто виконується в JavaScript для форматування введення користувачів, відображення імен чи заголовків та забезпечення консистенції текстів користувацького інтерфейсу.

## Як це зробити:
У JavaScript немає вбудованого методу для безпосереднього перетворення рядків з великої літери, але це легко реалізувати, використовуючи базові методи маніпуляцій з рядками.

### Використання стандартного JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Вивід: "Hello world"
```

### Версія ES6
За допомогою літералів шаблонів ES6 функцію можна записати більш лаконічним способом:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Вивід: "Hello ES6"
```

### Використання Lodash
Lodash - це популярна допоміжна бібліотека третьої сторони, яка пропонує широкий спектр функцій для маніпуляції та роботи з JavaScript значеннями, включно з рядками. Для перетворення рядка з великої літери за допомогою Lodash:
```javascript
// Спочатку встановіть lodash, якщо ви ще не зробили це: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Вивід: "Lodash example"
```
_Зверніть увагу, що Lodash не тільки перетворює першу літеру на велику, але й перетворює решту рядка на малі літери, що відрізняється від реалізації на чистому JavaScript._

### Використання CSS (Тільки для відображення)
Якщо мета - перетворити текст для відображення в UI, можна використовувати CSS:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Відображається як "Hello css" -->
```
**Примітка:** Цей метод змінює спосіб відображення тексту на веб-сторінці без змін самого рядка в JavaScript.
