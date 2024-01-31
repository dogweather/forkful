---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Регулярні вирази – це шаблони для пошуку та маніпуляції текстом. Програмісти використовують їх, щоб з легкістю знаходити, замінювати, або перевіряти наявність певних шаблонів у рядках.

## Як це зробити:
```javascript
// Пошук слів, що починаються на "в":
let regex = /\bв\w*/gi;
console.log("ведмідь весна вітер".match(regex));
// Виведе: ['ведмідь', 'весна', 'вітер']

// Заміна рядків:
let replaceRegex = /кіт/gi;
let result = "Кіт спить на килимку.".replace(replaceRegex, "пес");
console.log(result);
// Виведе: 'пес спить на килимку.'

// Валідація електронної адреси:
let emailRegex = /^\w+@\w+\.\w+$/;
let emailToTest = "example@test.com";
console.log(emailRegex.test(emailToTest));
// Виведе: true
```

## Поглиблений огляд
Регулярні вирази (RegEx) виникли у 1950-х. Стівен Кліні з MIT розробив базові концепції, що лягли в основу багатьох сучасних інструментів текстової обробки. Альтернативи RegEx, такі як парсери рядків та вбудовані мовні методи, можуть бути кращі в конкретних випадках, але менш гнучкі. При роботі з RegEx у JavaScript важливо розуміти флаги, групування та класи символів, щоб збільшити ефективність і точність пошуку.

## Дивіться також
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExp Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Regex101: Online Regex Tester and Debugger](https://regex101.com/)
- [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
