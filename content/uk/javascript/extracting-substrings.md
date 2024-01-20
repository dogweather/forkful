---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке й навіщо це потрібно?
Витягування підрядка, це процес отримання частини рядка з існуючого рядка в JavaScript. Програмісти роблять це, щоб зробити код більш читабельним і легким для розуміння, видаляючи зайві і непотрібні частини.

## Як це робити:
В JavaScript є два зручних методи для витягування підрядків: `slice` і `substring`.

```Javascript
let str = "Привіт, це ваш JavaScript код";
let substr = str.slice(8, 10); 
console.log(substr);
```

Цей кусок коду виведе `це` в консолі.

```Javascript
let str = "Привіт, це ваш JavaScript код";
let substr = str.substring(8, 10); 
console.log(substr);
```

Тут також виведеться `це`.

Різниця між `slice` і `substring` полягає в поведінці при введенні від'ємних значень або значень, що перевищують довжину рядка.

## Поглиблено
Витягування підрядків, як і багато інших речей в JavaScript, історично базується на C-подібних мовах програмування. Ці мови вплинули на дизайн і семантику JavaScript. 

Щодо альтернатив, ви також можете використовувати метод `substr`, хоча він вважається застарілим і не рекомендується для використання в новому коді.

Щодо виконання: як `slice`, так і `substring` виконують майже однаково. Основна різниця полягає в тому, як вони поводяться з від'ємними індексами або індексами, які перевищують довжину рядка.

## Дивіться також
- [Документація Mozilla про slice](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Документація Mozilla про substring](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Документація Mozilla про substr](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/substr)