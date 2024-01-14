---
title:                "TypeScript: Порівняння двох дат."
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Перевірка та порівняння дат є необхідними завданнями під час програмування. Це допомагає визначати часові інтервали, створювати умови та виконувати певні дії. Наприклад, порівняння дат може бути корисним при реалізації функцій нагадування або при розрахунку строків дії промо-кодів. У цій статті ми розглянемо як здійснити порівняння двох дат за допомогою TypeScript.

## Як це зробити

Для початку ми створимо дві змінні типу Date, які міститимуть дати, які хочемо порівняти. Наприклад:

```TypeScript
let firstDate: Date = new Date('2021-01-01');
let secondDate: Date = new Date('2021-01-15');
```

Далі, використовуючи умовний оператор if, ми можемо перевірити, яка з дат є більшою або чи вони дорівнюють одна одній. Наприклад:

```TypeScript
if (firstDate > secondDate) {
  console.log("Перша дата більша за другу");
} else if (firstDate < secondDate) {
  console.log("Перша дата менша за другу");
} else {
  console.log("Дати рівні");
}
```

Якщо потрібно перевірити, чи дата є більшою або дорівнює певній даті, можна використати метод `getTime()`, що поверне кількість мілісекунд, які пройшли з 1 січня 1970 року. Наприклад:

```TypeScript
let thirdDate: Date = new Date('2020-01-01');
let fourthDate: Date = new Date('2020-01-01');
if (thirdDate.getTime() >= fourthDate.getTime()) {
  console.log("Третя дата більша або дорівнює четвертій");
}
```

Вивід: "Третя дата більша або дорівнює четвертій"

## Глибока підготовка

У TypeScript існує можливість використовувати оператори порівняння (`>`, `<`, `>=`, `<=`) для об'єктів типу Date, оскільки вони підтримують приведення типів. Це означає, що можемо виконувати порівняння з датами так, як це робиться з числами. Також, варто зазначити, що метод `getTime()` повертає значення типу number, тому його теж можна використати для порівняння.

## Дивіться також

- [Робота з датами в TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
- [Оператори порівняння в TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#comparison-operators)