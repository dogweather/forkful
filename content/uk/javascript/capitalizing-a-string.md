---
title:                "Javascript: Капіталізація рядка"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Тому чому

В програмуванні часто потрібно змінювати рядки, щоб вони виглядали краще чи були у визначеному форматі. Один з таких змін - це капіталізація рядка, коли перша буква стає великою. Наприклад, з рядка "javascript" ми зможемо створити "Javascript". Це може бути корисно для відображення назви країни, міста, підзаголовків і багатьох інших випадків.

## Як це зробити

Існує кілька способів капіталізувати рядок в Javascript, але найбільш зручний і простий - використовувати вбудовані методи. Наприклад, для першої букви рядка "javascript" ми можемо застосувати метод `toUpperCase()`:

```Javascript
let string = "javascript";
let capitalizedString = string[0].toUpperCase() + string.slice(1);
console.log(capitalizedString); // Вивід: Javascript
```

Цей код спочатку вибирає перший символ рядка за допомогою квадратних дужок. Потім застосовує метод `toUpperCase()` до цього символу, який перетворює його на велику літеру. Нарешті, за допомогою методу `slice()` ми додаємо до першого символу решту рядка, щоб отримати капіталізований рядок.

Існує також вбудований метод `replace()` для зміни певних символів в рядку. Його можна застосувати до нашого прикладу з такими аргументами:

```Javascript
let string = "javascript";
let capitalizedString = string.replace(string[0], string[0].toUpperCase());
console.log(capitalizedString); // Вивід: Javascript
```

Цей код замінює перший символ рядка на той же символ, але з великою літерою, що призводить до капіталізації.

## Глибші деталі

Існує багато інших методів і підходів для капіталізації рядка в Javascript. Наприклад, ви можете створити власну функцію, яка буде перетворювати рядок так, як ви потребуєте. Також варто згадати, що капіталізація не обмежується лише першою буквою, ви можете застосувати її до всього рядка або навіть до окремих слів.

## Дивись також

- [Стаття про метод `toUpperCase()` на сайті W3Schools](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [Стаття про метод `replace()` на сайті W3Schools](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Стаття про капіталізацію рядка на сайті MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)