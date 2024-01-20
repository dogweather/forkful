---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Видалення символів, що відповідають певному шаблону - це пошук конкретних елементів в рядках коду і їх видалення. Програмісти це роблять для оптимізації коду і виправлення помилок.

## Як це зробити:

Одним з простих методів є використання вбудованого методу `replace()` в JavaScript. Він шукає певний шаблон і замінює його. Щоб видалити зайве, ми просто замінимо наївність:

```Javascript
    let my_string = 'This is a test string';
    let new_string = my_string.replace('test', '');
    console.log(new_string);
```

Вихід: `This is a  string`

## Занурення в деталі:

1. **Історичний контекст**: JavaScript був створений в 1995 році, і з того часу він пропонує різні способи видалення символів. Значно пізніше додали `replace()`.
2. **Альтернативи**: Інша популярна функція JavaScript є `split()` і `join()`. `split()` розбиває рядок на масив по заданому символу, а `join()` об'єднує масив назад в рядок.
    
```Javascript
    let my_string = 'This is a test string';
    let new_string = my_string.split('test').join('');
    console.log(new_string);
```

Вихід: `This is a  string`
3. **Деталі імплементації**: Проте, при використанні `replace()`, за замовчуванням замінюється лише перша зустріч з шаблоном. Якщо ви хочете замінити всі збіги, використовуйте регулярні вирази з глобальним прапором /g.

```Javascript
    let my_string = 'This test is a test string';
    let new_string = my_string.replace(/test/g, '');
    console.log(new_string);
```

Вихід: `This  is a  string`

## Дивіться також:

1. [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [MDN Web Docs: String.prototype.split()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/split)
3. [MDN Web Docs: Array.prototype.join()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Array/join)