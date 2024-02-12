---
title:                "Використання регулярних виразів"
aliases:
- /uk/typescript/using-regular-expressions/
date:                  2024-02-03T19:18:48.346792-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Регулярні вирази, або regex, це потужний інструмент для пошуку збігів та пошуку у програмуванні. Програмісти використовують regex для завдань, як-от перевірка даних користувача, пошук тексту чи маніпуляції з рядками, оскільки він ефективний та універсальний.

## Як це зробити:

Давайте зануримось у TypeScript і подивимось, як regex використовується для розповсюджених завдань.

```TypeScript
// Визначення регулярного виразу для електронної адреси
const emailPattern = /\S+@\S+\.\S+/;

// Перевірка, чи рядок відповідає шаблону електронної пошти
const email = "user@example.com";
console.log(emailPattern.test(email)); // Вивід: true

// Знаходження і заміна цифр у рядку
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Вивід: "Item # costs $#"

// Видобування конкретних частин з рядку, використовуючи групи захоплення
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // Вивід: "April" "10" "2021"
```

## Глибоке занурення

Ще в 1950-х роках математик Стівен Клін описав регулярні вирази як модель для представлення регулярних мов, яка згодом стала необхідною в комп'ютерних науках. Прискорившись в часі, regex є універсальним у програмуванні для роботи з текстом.

Хоча regex є швейцарським ножем для операцій з рядками, він не без альтернатив. Залежно від складності завдання, іноді методи рядків, такі як `includes()`, `startsWith()`, `endsWith()`, або навіть розбір за допомогою бібліотеки можуть бути кращими. Наприклад, розбір складного JSON-рядка за допомогою regex може бути кошмаром — використовуйте парсер JSON замість цього.

Що стосується реалізації, regex у JavaScript і TypeScript базується на специфікації мови ECMAScript. За лаштунками, двигуни використовують автомати станів для ефективного пошуку збігів. Варто зазначити, що операції з регулярними виразами можуть бути дорогими з точки зору продуктивності, особливо з погано написаними шаблонами — остерігайтеся "катастрофічного повернення назад".

## Дивіться також

- MDN Web Docs про регулярні вирази: [MDN Регулярні Вирази](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Інструмент для тестування і налагодження шаблонів regex [Regex101](https://regex101.com/)
- Книга "Mastering Regular Expressions" для глибокого розуміння: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
