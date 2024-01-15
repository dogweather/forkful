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

## З чого почати

Відчуваєш потребу у зменшенні регістру символів у рядку? Немає нічого зайвого у такому бажанні! У даній статті ми розглянемо, як саме виконати цю задачу за допомогою Javascript. Це необхідно, наприклад, для перевірки рівності двох рядків в незалежності від їх регістру або для форматування користувацьких введених даних.

## Як це зробити

Для початку, нам потрібно вибрати самий простий спосіб конвертування рядку до нижнього регістру - використання методу `.toLowerCase()`. Давайте переглянемо приклад коду, як це можна зробити за допомогою цього методу:

```Javascript
let inputString = "Hello, World!";
let lowerCaseString = inputString.toLowerCase();
console.log(lowerCaseString);
```

На виході ми отримаємо `"hello, world!"` - рядок із зменшеним регістром, який можна використати для подальшої обробки або виведення.

## Глибше

Слід зазначити, що метод `.toLowerCase()` не міняє оригінального рядка, а повертає новий рядок із зменшеним регістром. Це може бути важливим у випадку, якщо потрібно зберегти оригінальний рядок. Давайте подивимось на сталу доступну у Javascript - `"DЕІfo"`:

```Javascript
const str = "DЕІfo";
console.log(str.toLowerCase()); // dеіfo
console.log(str); // DЕІfo
```

Як бачимо, приклад використання методу `.toLowerCase()` також демонструє ситуацію, коли символ "i" стає кирилицею після конвертації до нижнього регістру.

## Дивись також

* [MDN документація про метод `.toLowerCase()`](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
* [Вступ до роботи з рядками у Javascript](https://www.w3schools.com/js/js_strings.asp)