---
title:                "З'єднання рядків"
html_title:           "Javascript: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому 

У логічному виразі потрібно об'єднувати значення зі стрічками. Це є одним з інструментів, який дозволяє створювати динамічний вміст на веб-сторінках та полегшує роботу зі змінними даними. 

## Як
Існує кілька способів здійснити об'єднання стрічок у Javascript. Найпростіший спосіб - використання "+" оператора. Нижче наведено приклад коду та його вивід: 

```Javascript
let str1 = "Hello";
let str2 = "world";

let result = str1 + str2;
console.log(result);

// Output: HelloWorld
```

Також можна скористатися методом `concat()`, який дозволяє об'єднувати більше ніж дві стрічки. Приклад використання та вивід: 

```Javascript
let str1 = "Hello";
let str2 = "world";
let str3 = "!";

let result = str1.concat(str2, str3);
console.log(result);

// Output: HelloWorld!
```

Окрім цього, у JavaScript є можливість використовувати динамічне об'єднання за допомогою шаблонних літералів. Приклад та вивід: 

```Javascript
let str1 = "Hello";
let str2 = "world";

let result = `${str1} ${str2}`;
console.log(result);

// Output: Hello world
```

## Глибше
Під час об'єднання стрічок у Javascript, важливо пам'ятати про типи даних. Якщо хоча б одне зі значень є числовим, воно буде перетворено у стрічку під час об'єднання. Наприклад:

```Javascript
let str = "Hello";

let result = str + 5;
console.log(result);

// Output: Hello5
```

Крім того, зверніть увагу, що оператор "+" можна використовувати для об'єднання не тільки стрічок, але й масивів. Наприклад: 

```Javascript
let str1 = "Hello";
let str2 = "world";
let arr1 = [1, 2, 3];

let result = str1 + str2 + arr1;
console.log(result);

// Output: Helloworld1,2,3
```

## Дивіться також
- [MDN документація щодо об'єднання стрічок у Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools Tutorial: Concatenating Strings in JavaScript](https://www.w3schools.com/js/js_string_concat.asp)
- [JavaScript.info: Strings](https://javascript.info/types#strings)