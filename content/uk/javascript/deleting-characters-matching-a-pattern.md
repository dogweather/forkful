---
title:                "Javascript: Видалення символів, що відповідають шаблону"
simple_title:         "Видалення символів, що відповідають шаблону"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

JavaScript є однією з найпопулярніших мов програмування для створення веб-застосунків та веб-сайтів. Часто виникає ситуація, коли нам потрібно видалити певні символи з рядка відповідно до певного шаблону. Це може бути корисно для перетворення даних або очищення введення від користувача. Таким чином, знання про видалення символів за певним шаблоном є корисним для будь-якого JavaScript розробника.

## Як

Для видалення символів за певним шаблоном в JavaScript, ми можемо використовувати метод `replace()` рядків. Синтаксис цього методу виглядає так:

```
str.replace(regexp, replacement)
```

Тут `str` - це рядок, в якому ми хочемо видалити символи, `regexp` - регулярний вираз, який визначає шаблон для видалення, а `replacement` - символи, які будуть використовуватися для заміни.

Наприклад, якщо ми хочемо видалити всі числа з рядка, ми можемо використовувати наступний код:

```
let str = "1a2b3c4d";
let replaced = str.replace(/[0-9]/g, '');
console.log(replaced); // виведе "abcd"
```

У даному прикладі ми використовуємо регулярний вираз `/[0-9]/g`, який визначає всі цифри від 0 до 9. І за допомогою порожнього рядку в `replacement`, ми видаляємо всі ці символи з рядка.

## Глибоке поглиблення

У JavaScript, регулярні вирази можуть бути складними та потужними інструментами. Вони дозволяють визначати шаблони для пошуку та заміни певних символів у рядках. Для того, щоб розуміти їх повністю, потрібно вивчати регулярні вирази окремо. Також, можна використовувати функцію `match()` для знаходження всіх відповідних символів або `split()` для поділу рядка на підрядки за певним шаблоном.

## Дивіться також

- [Регулярні вирази в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Метод replace() рядків](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Функція match() рядків](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/match)
- [Функція split() рядків](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/split)