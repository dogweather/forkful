---
title:                "Gleam: Капіталізація рядка"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Зазвичай нам потрібно привести рядок до великої літери для відповідності заголовкам або виправлення неправильного вводу. Gleam має унікальну функцію для цього, яка робить цей процес швидким і зручним.

## Як

Щоб скористатися цією функцією в Gleam, ми можемо використовувати вбудовану функцію capitalize, яка приймає рядок як аргумент і повертає новий рядок з першої літери, перетвореної на велику.

```Gleam
let name = "gleam"
let capitalizedName = capitalize(name)

gleam // вихід: Gleam
```

Додатково, ми можемо використовувати цю функцію разом з іншими функціями рядків, наприклад, split і replace, для більш складних маніпуляцій з рядками.

```Gleam
let sentence = "hello, world"
let capitalizedSentence = sentence
  |> split(",")
  |> List.map(capitalize)
  |> List.reduce("", "+")
  |> replace("+ ", "")

capitalizedSentence // вихід: Hello World
```

## Глибокий занурення

Ця функція також містить як ім'я так і опціональний другий аргумент, який приймає булеве значення. Якщо другий аргумент встановлений на true, перша літера перетвориться на велику, а всі інші літери на маленькі.

```Gleam
let name = "gLeAm"
let capitalizedName = capitalize(name, true)

capitalizedName // вихід: Gleam
```

Це корисно, коли ми хочемо привести рядок до однорідної форми, не зважаючи на те, як він був введений.

## Дивіться також

- [Документація Gleam](https://gleam.run/documentation)
- [Приклади коду Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Стаття про функції рядків в Gleam](https://medium.com/functional-programming-in-gleam/string-manipulation-in-gleam-17f459b843d4)