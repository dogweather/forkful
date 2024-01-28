---
title:                "Видалення лапок зі строки"
date:                  2024-01-26T03:43:25.596007-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Видалення лапок з рядка означає вилучення оточуючих одинарних (`'`) або подвійних (`"`) символів лапок, що визначають літерали рядків у коді. Програмісти роблять це з кількох причин, таких як форматування виводу, санітація вводу користувача або підготовка рядків до аналізу або зберігання, де лапки непотрібні або можуть спричинити помилки.

## Як:
Ось ваш гід без зайвих слів, як позбутися тих надокучливих лапок у ваших рядках в TypeScript.

```typescript
// Варіант А: Заміна одинарних або подвійних лапок за допомогою regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// Варіант Б: Робота з рядками, що починаються і закінчуються різними лапками
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// Варіант В: Видалення кількох типів лапок
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Поглиблений Огляд
Далеко назад, ще до того як TypeScript став реальністю, програмісти JavaScript вже стикалися з хитрощами лапок, і історія залишається майже тією самою для TypeScript. Часи змінюються, і способи роботи з рядками також. Нині, з використанням потужності regex, ми оминаємо використання незграбного розбиття рядків або інших нудних методів.

Хоча вищезазначені приклади мають покрити більшість ваших потреб, пам'ятайте, що лапки можуть бути складними. Вкладені, невідповідні та екрановані лапки - ось хитруни, що чекають, щоб спіткнути вас. Для цих випадків може знадобитися більш складні шаблони або навіть парсери, щоб впоратися з кожним хитрим випадком.

Альтернативи? Деякі люди вважають за краще використовувати бібліотеки, як-от lodash, з методами, такими як `trim` і `trimStart` / `trimEnd`, які можна налаштувати, щоб обрізати лапки, якщо ви встановите символи, які хочете відрізати.

А для ентузіастів TypeScript, не забуваємо про типи. Хоча тут ми в основному працюємо з рядками, коли ви працюєте з вводом користувача або аналізуєте, використання охоронців типу або навіть узагальнень може допомогти вам забезпечити безпеку вашого коду так само, як і обрізані лапки.

## Дивіться Також
Перегляньте ці віртуальні місця для отримання більшої інформації:

- MDN Web Docs про regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Офіційна Документація TypeScript (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Помічники для Рядків (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Пройдіть через окопи, де безліч розробників боролися з катастрофами лапок (https://stackoverflow.com/)
