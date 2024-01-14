---
title:                "TypeScript: Капіталізація рядка."
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Навіщо

Перший крок до інтерактивної програмної розробки - це володіти фундаментальними навичками програмування. Один з найпростіших, але дуже корисних прийомів - це вивчення методів зміни текстових рядків. Одним з них є метод capitalize(), який робить першу літеру будь-якого слова великою.

## Як зробити

Для початку, потрібно оголосити змінну зі стрічкою, яку потрібно змінити. Далі, застосуйте до неї метод capitalize() і збережіть результат у нову змінну. Тепер, просто виведіть нову змінну на екран, щоб переконатись у правильності дії методу.

```TypeScript
let exampleString: string = "приклад";
let capitalizedString: string = exampleString.capitalize();
console.log(capitalizedString);
```

Результат: "Приклад"

## Глибинна розробка

Метод capitalize() використовує внутрішні функції JavaScript, які перетворюють першу літеру рядка на велику. Це дозволяє легко і швидко зробити текст більш читабельним, що є дуже важливим у програмуванні.

Також варто зазначити, що метод capitalize() може бути застосований не лише до простих стрічок, але й до складніших об'єктів, що дозволяє зробити більш гнучким його використання в програмі.

## Дивіться також

- [Документація TypeScript](https://www.typescriptlang.org/docs)
- [Розділ про рядки в документації JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)