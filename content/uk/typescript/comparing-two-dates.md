---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Порівняння двох дат в TypeScript

## Що та чому?
Порівняння дат - це процес визначення, яка з двох дат раніша або пізніша. Це необхідно робити, коли програмісти потребують з'ясувати, наприклад, чи минув термін дії чогось, або щоб планувати майбутні події.

## Як зробити:
В TypeScript порівняння дат виконується просто та інтуїтивно зрозуміло. Ось приклад:

```TypeScript
let date1 = new Date('2021-07-01');
let date2 = new Date('2022-07-01');

if(date1 < date2) { 
  console.log('Дата1 раніше дати2'); 
} else if(date1 > date2) { 
  console.log('Дата1 пізніше дати2'); 
} else { 
  console.log('Дата1 та дата2 однакові'); 
}
```
Вихідний код: 
```
'Dата1 раніше дати2'
```
## Поглиблений огляд:
Історично, порівняння дат було складнішим завданням, але нові версії TypeScript дуже спростили цей процес. Якщо вам потрібні більш детальні порівняння, наприклад, порівняння в мілісекундах, можна використовувати методи getTime() або ValueOf(). 

## Додатково:
1. Документація Date об'єктів в JavaScript: [MDN Web Docs](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. Бібліотека для роботи з датами та часом: [Moment.js](https://momentjs.com/)
3. Бібліотека для роботи з датами в TypeScript: [date-fns](https://date-fns.org/) 

Будь ласка, зауважте, що деякі з цих ресурсів можуть бути англійською мовою.