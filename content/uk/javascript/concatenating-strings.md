---
title:                "Javascript: З'єднання рядків"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

Почему: Объясняючи *чому* людина мала би заангажуватися у з'єднання рядків.

З'єднання рядків є важливою властивістю у програмуванні, оскільки воно дозволяє об'єднати декілька рядків у один, що полегшує роботу з даними та додаванням тексту до програми.

Простий приклад:

```Javascript
let firstName = "Олена";
let lastName = "Петренко";
let fullName = firstName + " " + lastName;
// fullName = "Олена Петренко";
```

## Як використовувати

З'єднання рядків у Javascript можна виконати за допомогою оператора "+" або методу "concat()". Однак, якщо розглядати більш складні випадки, можна використовувати регулярні вирази або вбудовану функцію "join()".

Приклади:

```Javascript
let text = "Привіт";
text += "!";
// text = "Привіт!"

let greetings = ["Привіт", "Доброго дня", "Вітаю"];
console.log(greetings.join(", "));
// output = "Привіт, Доброго дня, Вітаю"
```

## Глибоке дослідження

У Javascript, рядки використовуються як об'єкти, тому вони мають доступ до різних методів. Наприклад, метод "split()" дозволяє розбити рядок на масив за допомогою вказаного розділювача. Також, можна використовувати спеціальні символи, такі як шаблонні рядки (template literals) для покращення роботи з рядками.

Приклади:

```Javascript
let sentence = "Я люблю програмувати з Javascript";
console.log(sentence.split(" "));
// output = ["Я", "люблю", "програмувати", "з", "Javascript"]

let name = "Софія";
let greeting = `Привіт, моя ім'я ${name}`;
// greeting = "Привіт, моя ім'я Софія"
```

## Дивіться також

- [Документація з рядків в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Робота з рядками в Javascript: детальніше про методі "split()"](https://www.w3schools.com/jsref/jsref_split.asp)
- [Вбудована функція "join()" у Javascript](https://www.codegrepper.com/code-examples/javascript/js+join)