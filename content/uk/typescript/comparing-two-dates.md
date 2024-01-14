---
title:    "TypeScript: Порівняння двох дат."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Часто, в програмуванні ми зіштовхуємося з ситуаціями, коли нам потрібно порівняти дати. Це може бути необхідно під час форматування дати для виведення на екран або при роботі з діапазонами дат. У TypeScript є дуже зручний спосіб порівняння дат, який ми розглянемо у цій статті.

## Як це зробити

Для порівняння дат в TypeScript використовується метод `Date.prototype.getTime()`, який повертає кількість мілісекунд, які пройшли з 1 січня 1970 року 00:00:00 UTC до вказаної дати. Давайте подивимося на приклад коду, який допоможе нам зрозуміти цей метод.

```TypeScript
const firstDate = new Date('2020-01-01');
const secondDate = new Date('2020-01-02');

if (firstDate.getTime() < secondDate.getTime()) {
  console.log('Перша дата передує другої');
} else if (firstDate.getTime() > secondDate.getTime()) {
  console.log('Перша дата йде після другої');
} else {
  console.log('Дати рівні');
}
```

Результат виконання цього коду буде наступним:

```
Перша дата передує другої
```

Також можна порівнювати дати за допомогою операторів `>`, `<`, `>=`, `<=` або застосовуючи метод `Date.prototype.getTime()`, як у нашому прикладі.

## Огляд

Метод `Date.prototype.getTime()` також ідеально підходить для порівняння дат у різних часових зонах, оскільки він повертає значення відліку від UTC часу. Також варто знати, що при порівнянні дат за допомогою операторів, розрізняються як часові зони, так і дні або місяці (наприклад, дата 2020-01-02 буде більшою за дату 2020-01-01, будь-яка часова зона).

## Дивись також

- [Робота з датами в TypeScript](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Метод getTime()/Кількість мілісекунд](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [Офіційна документація TypeScript](https://www.typescriptlang.org/docs)