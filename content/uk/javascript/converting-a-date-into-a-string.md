---
title:                "Javascript: Перетворення дати в рядок"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

У конвертуванні дати в рядок є багато важливих випадків в програмуванні. Наприклад, коли ми хочемо вивести дату на екран користувачу або використовуємо її для іменування файлу. Конвертація дати в рядок дозволяє змінити формат дати для використання в різних ситуаціях.

## Як

```javascript
let date = new Date(2021, 9, 22); // створюємо об'єкт дати
let stringDate = date.toLocaleDateString("uk-Uk",{ // конвертуємо дату в рядок з українським форматом
  year: 'numeric', 
  month: 'long', 
  day: 'numeric'
});
console.log(stringDate); // виводимо результат: "22 жовтня 2021 р."
```

## Глибокий аналіз

У Javascript є різні методи для конвертування дати в рядок. Метод `toLocaleDateString()` дозволяє налаштувати формат дати, включаючи мову та вид дати (довгий, короткий чи числовий). Можна також використовувати методи `getFullYear()`, `getMonth()` та `getDate()` для отримання окремих частин дати і об'єднувати їх в рядок за допомогою конкатенації.

## Дивіться також

- [MDN - метод `toLocaleDateString()`](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [W3Schools - Javascript Date об'єкт](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Stack Overflow - як конвертувати дату в рядок в Javascript](https://stackoverflow.com/questions/3552461/how-to-format-a-javascript-date)