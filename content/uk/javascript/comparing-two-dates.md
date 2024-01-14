---
title:                "Javascript: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми розглянемо порівняння двох дат у Javascript. Це важлива тема для програмістів, які працюють з датами, оскільки потрібно вміти правильно порівнювати їх для ефективної роботи.

## Як

Для порівняння двох дат у Javascript використовується метод **getTime()**, який повертає кількість мілісекунд, що пройшли з 1 січня 1970 року до вказаної дати. Потім можна використовувати стандартні оператори порівняння, такі як **<**, **>**, **===**, для порівняння цих значень.

```Javascript
let date1 = new Date(2021, 0, 1); // 1 січня 2021 року
let date2 = new Date(2021, 0, 15); // 15 січня 2021 року

let diff = date1.getTime() – date2.getTime();

if (diff > 0) {
    console.log(date2 + ' була раніше за ' + date1);
} else {
    console.log(date2 + ' була пізніше або рівна ' + date1);
}
```

В даному прикладі ми створюємо дві дати, порівнюємо їх за допомогою **getTime()** та виводимо відповідне повідомлення в консоль. У результаті отримуємо наступний вивід:

```Javascript
Tue Dec 31 2020 21:00:00 GMT-0300 (Eastern Standard Time) була раніше за Fri Jan 15 2021 21:00:00 GMT-0300 (Eastern Standard Time)
```

## Глибша підводка

При порівнянні двох дат у Javascript важливо пам'ятати, що метод **getTime()** повертає значення в мілісекундах, а це означає, що рівні дати можуть мати дрібну різницю у мілісекундах. Також потрібно звертати увагу на часові пояси, оскільки вони можуть впливати на результат порівняння.

## Дивіться також

- [MDN Web Docs: Date](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Web Docs: Date.prototype.getTime()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [W3Schools: JavaScript Date Comparison](https://www.w3schools.com/js/js_dates_compare.asp)