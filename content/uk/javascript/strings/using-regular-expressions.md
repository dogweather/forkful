---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:33.799540-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (\u0430\u0431\u043E regex) \u0432 JavaScript \u2014 \u0446\u0435\
  \ \u0448\u0430\u0431\u043B\u043E\u043D\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\
  \u044F \u043F\u043E\u0448\u0443\u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\
  \u0430\u0446\u0456\u0439 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443\
  \ \u0440\u044F\u0434\u043A\u0430\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C \u0457\u0445 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.975640-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\
  \u0430\u0437\u0438 (\u0430\u0431\u043E regex) \u0432 JavaScript \u2014 \u0446\u0435\
  \ \u0448\u0430\u0431\u043B\u043E\u043D\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C\u0441\u044F \u0434\u043B\
  \u044F \u043F\u043E\u0448\u0443\u043A\u0443 \u043A\u043E\u043C\u0431\u0456\u043D\
  \u0430\u0446\u0456\u0439 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432 \u0443\
  \ \u0440\u044F\u0434\u043A\u0430\u0445."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Що та чому?

Регулярні вирази (або regex) в JavaScript — це шаблони, що використовуються для пошуку комбінацій символів у рядках. Програмісти використовують їх для пошуку, екстракції та маніпулювання текстом, дозволяючи виконувати потужні операції з обробкою рядків за допомогою стислого коду.

## Як:

### Базовий пошук

Для початку можна створити простий regex-шаблон і використовувати його для пошуку відповідностей у рядку. Тут ми знайдемо слово "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Використання `String.prototype.match()`

Для отримання масиву відповідностей:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Глобальний пошук

Для пошуку всіх відповідностей використовуйте прапорець `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Пошук без врахування регістра

Прапорець `i` ігнорує регістр:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Заміна тексту

Використовуйте `String.prototype.replace()`, щоб замінити частини рядка:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### Використання груп

Групи можуть захоплювати частини шаблону:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### Сторонні бібліотеки

Хоча вбудовані можливості regex в JavaScript є потужними, деякі завдання можна спростити за допомогою бібліотек, таких як `XRegExp`. Вона пропонує додатковий синтаксис та прапорці, роблячи складні шаблони більш зрозумілими:

```javascript
// Приклад використання бібліотеки XRegExp
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

Цей фрагмент демонструє використання `XRegExp` для пошуку всіх Unicode слів у рядку, демонструючи здатність бібліотеки обробляти розширені набори символів, які виходять за межі вбудованих можливостей JavaScript.
