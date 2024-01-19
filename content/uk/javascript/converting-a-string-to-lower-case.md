---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Перетворення рядка на малі букви - це процес, коли всі великі букви в рядку замінюються на малі. Програмісти роблять це, коли хочуть порівняти два рядки без урахування регістра.

## Як зробити:

В JavaScript, один із способів зробити це - використати метод `.toLowerCase()`. Ось приклад:

```Javascript
let message = "Привіт, Друже!";
let lowerCaseMessage = message.toLowerCase();

console.log(lowerCaseMessage); 
// "привіт, друже!"
```
Код вище змінює весь текст на малі літери та виводить у консоль "привіт, друже!".

## Поглиблений аналіз

Метод `.toLowerCase()` був частиною оригінального стандарту ECMAScript в 1997 році. Це робить його надійним і стабільним способом виконувати такі операції.

Як альтернатива, можливо використовувати регулярні вирази або петлю for, але це вимагає більше коду та може призвести до помилок.

Якщо глибше погрузитися в деталі, метод `.toLowerCase()` працює з Unicode та розуміє локалізовані символи, будучи важливим інструментом для роботи з інтернаціоналізованими рядками.

## Дивіться також:

Є багато ресурсів з додатковими поясненнями та прикладами коду. Ось кілька з них:

1. MDN Web Docs: String.prototype.toLowerCase() (https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. JavaScript Info: Рядки (https://javascript.info/string)
3. W3schools: JavaScript String toLowerCase() Method (https://www.w3schools.com/jsref/jsref_tolowercase.asp)