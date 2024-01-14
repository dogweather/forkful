---
title:                "Javascript: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Для багатьох програмістів конвертація рядка до нижнього регістру може здаватися дуже простим завданням, проте цей процес має велике значення для правильної роботи програми. Перетворення рядка до нижнього регістру робить його більш читабельним та зручним для подальшої обробки.

## Як

```Javascript
let str = "HELLO WORLD";
console.log(str.toLowerCase());
// Output: hello world
```

У цьому прикладі ми використали метод `toLowerCase()` для зміни рядка з великих літер до нижнього регістру. Цей метод повертає новий рядок зі зміненим регістром, тому присвоїли його до змінної `str`. Потім використали консоль для виведення отриманого результату.

## Глибока аналітика

Крім методу `toLowerCase()`, існує ще кілька способів, якими можна змінити регістр рядка в Javascript. Один з них - використання циклу `for` та методу `charAt()` для перебирання кожного символу рядка та зміни його регістру. Також існує метод `toUpperCase()`, який перетворює рядок до верхнього регістру.

Варто пам'ятати, що методи `toLowerCase()` та `toUpperCase()` не змінюють оригінальний рядок, а повертають новий. Тому, якщо потрібно зберегти змінений рядок, його необхідно присвоїти до нової змінної.

## Дивись також

- [Mozilla Developer Network - String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools - JavaScript String.toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [GeeksforGeeks - JavaScript String toLowerCase() Method](https://www.geeksforgeeks.org/javascript-string-tolowercase-method/)