---
title:                "Використання регулярних виразів"
aliases:
- /uk/javascript/using-regular-expressions/
date:                  2024-02-03T19:17:33.799540-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
