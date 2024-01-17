---
title:                "Видалення символів, які відповідають заданому шаблону"
html_title:           "Javascript: Видалення символів, які відповідають заданому шаблону"
simple_title:         "Видалення символів, які відповідають заданому шаблону"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що та навіщо? 
Видалення символів, які відповідають вказаному шаблону, є популярним завданням у програмуванні. Програмісти зазвичай виконують цю задачу, щоб очистити вхідні дані, встановити певні обмеження або перетворити рядки у більш зручний формат.

## Як це зробити: 
Для видалення символів за шаблоном використовуємо метод `replace()` в Javascript. Далі вказуємо шаблон і рядок, в якому потрібно зробити заміну. Наприклад, якщо ми хочемо видалити всі цифри із рядка "Hello123World", то застосовуємо наступний код: 
```Javascript 
var str = "Hello123World"; 
var newStr = str.replace(/[0-9]/g, ''); 
console.log(newStr); // виведе "HelloWorld" 
```

## Глибше вдивимося:
Історичний контекст цієї задачі пов'язаний з регулярними виразами - механізмом для пошуку та заміни символів у рядках. Однак, наявні є й інші способи видалення символів за шаблоном, такі як застосування циклів та умовних операцій. Реалізація цього завдання в Javascript залежить від використання глобального модифікатора `/g` і регулярних виразів, що дозволяє докладніше налаштувати процес видалення символів.

## Дивіться також:
- Документація по методу `replace()` у Javascript: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace 
- Регулярні вирази в Javascript: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/RegExp 
- Інші методи для роботи з рядками у Javascript: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String