---
title:                "Використання регулярних виразів"
html_title:           "TypeScript: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Регулярні вирази є потужним інструментом для роботи з текстом в програмуванні TypeScript. Вони дозволяють шукати, замінювати та валідувати певні шаблони у рядках, що дозволяє значно спростити роботу з текстовими даними.

## Як

```TypeScript
// Приклад застосування регулярного виразу для пошуку у тексті
const text = "Це приклад рядка, що містить дату: 12/05/2021.";
const regex = /\d{2}\/\d{2}\/\d{4}/; // шаблон для пошуку дати у форматі DD/MM/YYYY
const result = regex.exec(text); // результат: ["12/05/2021"]
```

```TypeScript
// Регулярні вирази також дозволяють виконувати заміни у рядках
const text = "Це приклад рядка, що містить дату в форматі MM-DD-YYYY.";
const regex = /\d{2}-\d{2}-\d{4}/; // шаблон для пошуку дати у форматі DD/MM/YYYY
const result = text.replace(regex, "YYYY-MM-DD"); // результат: "Це приклад рядка, що містить дату в форматі YYYY-MM-DD."
```

```TypeScript
// Регулярні вирази також можна використовувати для валідації даних
function validateEmail(email: string): boolean {
  const regex = /[\w-]+@([\w-]+\.)+[\w-]+/; // шаблон для перевірки формату email
  return regex.test(email);
}

const isValidEmail = validateEmail("test@example.com"); // результат: true
```

## Глибоке занурення

Процес створення та використання регулярних виразів може здатися складним на початку. Однак, вони є надзвичайно потужним інструментом, який дозволяє ефективно працювати з текстом у програмуванні. Будьте впевнені, що ви розумієте основи регулярних виразів та потренуйтеся практикувати їх використання. Це допоможе вам стати більш впевненими в знаходженні та роботі з текстовими даними.

## Див. також

- [Документація TypeScript](https://www.typescriptlang.org/docs/)
- [Регулярні вирази в TypeScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Відеоуроки з регулярних виразів в TypeScript](https://www.youtube.com/watch?v=MLEzkMMYpTs)