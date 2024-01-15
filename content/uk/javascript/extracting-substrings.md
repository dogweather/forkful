---
title:                "Видобування підрядків"
html_title:           "Javascript: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

##Чому

Завдання витягування підрядка може бути корисне для тих, хто працює з текстовими даними і хоче вибрати певну частину інформації з рядка. Наприклад, це може бути корисно для упорядкування та опрацювання великих обсягів текстових даних.

##Як це зробити

```Javascript
let string = "Це рядок з якими-небудь даними";

// Витягнути підрядок починаючи з індексу 0 і до кінця рядка
let substring1 = string.substring(0);
console.log(substring1); // "Це рядок з якими-небудь даними"

// Витягнути підрядок починаючи з індексу 4 і до кінця рядка
let substring2 = string.substring(4);
console.log(substring2); // "рядок з якими-небудь даними"

// Витягнути підрядок починаючи з індексу 4 і до індексу 10 (не включаючи його)
let substring3 = string.substring(4, 10);
console.log(substring3); // "рядок "
```

Ви можете використовувати метод `substring()` для витягування підрядків з будь-якого рядка, задавши початковий та кінцевий індекси. Також, ви можете витягнути підрядок починаючи з початку або до кінця рядка, не задаючи кінцевий індекс.

##Глибока аналітика

Метод `substring()` повертає підрядок, який задовольняє задані індекси. Це аналогічно до методу `slice()`, але з однією важливою відмінністю - якщо перший індекс більше другого, метод `substring()` автоматично поміняє їх місцями. Використання методу `substring()` без параметрів повертає копію початкового рядка. Тобто, метод поверне підрядок починаючи з початку і до кінця рядка, що дозволяє оброблювати рядки з початку або видалити певну частину з кінця.

##Дивіться також

- [MDN документація про метод substring()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Витягнення підрядка в Javascript](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Реальні приклади застосування витягування підрядків](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring)