---
title:                "Elm: Видалення символів, що відповідають заданому шаблону"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Зачем

Розробка програмного забезпечення - це завжди процес, у якому ми продумуємо не тільки як досягти бажаного результату, але й як покращити і підтримати його в подальшому. Видалення символів, що відповідають певному шаблону, може бути одним із способів покращити наш код і зробити його більш ефективним.

## Як це зробити

Для початку, давайте створимо функцію `deleteMatching`, яка приймає два параметри: рядок і шаблон. Вона буде перевіряти кожен символ у рядку і видаляти ті, які відповідають шаблону. Наприклад, якщо ми будемо вводити рядок "Hello World!" та шаблон "l", функція поверне нам "Heo Word!".

```
Elm
deleteMatching : String -> String -> String
deleteMatching str pattern =
  String.filter (not << String.contains pattern) str
  
deleteMatching "Hello World!" "l"
-- Returns "Heo Word!"
```

Також ми можемо використовувати більш складні шаблони, наприклад, регулярні вирази. Для цього ми можемо використовувати модуль `Regex`, який надає більш потужні можливості для роботи з текстом.

```
Elm
import Regex

deleteMatching : String -> String -> String
deleteMatching str pattern =
  Regex.replace (Regex.regex pattern) (const "") str
  
deleteMatching "Hello World!" "[aeiou]"
-- Returns "Hll Wrld!"
```

## Глибоке занурення

Насправді, видалення символів, які відповідають певному шаблону, може бути корисним у багатьох ситуаціях. Наприклад, ви можете використовувати його для валідації введеного користувачем тексту або для очищення його перед подальшою обробкою.

Також, якщо у вас є багато рядків, які потрібно обробити, то ви можете створити більш ефективну функцію, яка пройде по кожному рядку і застосує `deleteMatching` до нього.

## Дивись також

- [Офіційна документація Elm](https://elm-lang.org/docs)
- [Регулярні вирази в Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Туторіал з Elm на Medium (українською)](https://medium.com/@Saxen/elm-ваш-новый-регулярный-язык-часть-1-4ede0d51c8fe)