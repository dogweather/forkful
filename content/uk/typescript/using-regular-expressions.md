---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Регулярні вирази - механізм пошуку та маніпуляції текстом. Програмісти використовують їх для заміни тексту, валідації вводу, пошуку і вилучення данних.

## Як це зробити:
```typescript
const text: string = "Телефон Олени: 067-123-4567, робочий: 098-765-4321.";
const regExp: RegExp = /\b(\d{3})-(\d{3})-(\d{4})\b/g;

// Пошук номерів телефонів
const matches = text.match(regExp);
console.log(matches);
// Вивід: ['067-123-4567', '098-765-4321']

// Заміна формату номера телефона
const formatted = text.replace(regExp, '+38 ($1) $2-$3');
console.log(formatted);
// Вивід: Телефон Олени: +38 (067) 123-4567, робочий: +38 (098) 765-4321.
```

## Поглиблений аналіз:
Регулярні вирази прийшли з теорії автоматів і були популяризовані у програмуванні UNIX інструментами типу grep. Альтернативи включають роботу з текстом через вбудовані функції мови, але вони часто менш гнучкі. При роботі з TypeScript, регулярні вирази реалізовані через вбудований об'єкт RegExp, який підтримує більшість можливостей ECMAScript специфікації.

## Більше інформації:
- MDN Web Docs: RegExp - [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- RegExp тести онлайн: [https://regex101.com/](https://regex101.com/)
- TypeScript Handbook: Regular Expressions - [https://www.typescriptlang.org/docs/handbook/2/objects.html#regex-patterns](https://www.typescriptlang.org/docs/handbook/2/objects.html#regex-patterns)