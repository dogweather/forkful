---
title:    "Rust: З'єднання рядків."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

З'єднання рядків є важливою технікою в розробці програмного забезпечення, яка дозволяє поєднувати різні значення для створення нових. У мові програмування Rust також є методи для з'єднання рядків, що дозволяє розробникам зручно працювати зі строками даних.

## Як

```Rust
let str1 = "Це";
let str2 = " із";
let str3 = " строки";
let str_concat = format!("{}{}{}", str1, str2, str3);
```

Вищенаведений код створить змінну "str_concat", яка міститиме з'єднання трьох рядків. Результатом буде "Це із строки". Також можна використати оператор "+", але це може бути менш зручним при більшій кількості строк для з'єднання.

```Rust
let str1 = "Це";
let str2 = " значення";
let str3 = " разом";
let str4 = " в строках";
let str_concat = str1 + str2 + str3 + str4;
```

## Глибоке дослідження

У мові Rust є також метод "push_str", який дозволяє додавати до існуючого рядка нове значення. Наприклад:

```Rust
let mut str = String::from("Це");
str.push_str(" інший рядок");
```

Це може бути корисно, коли необхідно додати значення до існуючого рядка, а не створювати новий. Також можна використовувати метод "join", який дозволяє з'єднати массив рядків в один рядок з першеї символів між ними.

```Rust
let str_arr = ["рядок", "з рядків"];
let str_concat = str_arr.join(" разом ");
```

## Див. також

- [Офіційна документація Rust про методи для строк](https://doc.rust-lang.org/std/string/index.html#methods-3)
- [Стаття про з'єднання строк в мові Rust на сайті Medium](https://medium.com/@buggersolver/how-to-concatenate-strings-in-rust-a6091f85bff8)
- [Книга "The Rust Programming Language" про роботу зі строками](https://doc.rust-lang.org/book/ch08-02-strings.html)