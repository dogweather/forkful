---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:55.670592-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Rust \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0433\u043E \u043C\u0435\u0442\u043E\u0434\u0430 \u0434\u043B\
  \u044F \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u044F \u043A\u0430\u0436\u0434\u043E\u0433\u043E \u0441\u043B\u043E\u0432\u0430\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0437\u0430\u0433\u043B\u0430\u0432\
  \u043D\u044B\u0435 \u0431\u0443\u043A\u0432\u044B, \u043D\u043E \u043C\u044B \u043C\
  \u043E\u0436\u0435\u043C \u043B\u0435\u0433\u043A\u043E \u0440\u0435\u0430\u043B\
  \u0438\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0432\u043E\u0439 \u0441\u043E\
  \u0431\u0441\u0442\u0432\u0435\u043D\u043D\u044B\u0439,\u2026"
lastmod: '2024-03-13T22:44:44.634766-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Rust \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0433\u043E \u043C\u0435\u0442\u043E\u0434\u0430 \u0434\u043B\u044F\
  \ \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u044F \u043A\u0430\u0436\u0434\u043E\u0433\u043E \u0441\u043B\u043E\u0432\u0430\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0437\u0430\u0433\u043B\u0430\u0432\
  \u043D\u044B\u0435 \u0431\u0443\u043A\u0432\u044B, \u043D\u043E \u043C\u044B \u043C\
  \u043E\u0436\u0435\u043C \u043B\u0435\u0433\u043A\u043E \u0440\u0435\u0430\u043B\
  \u0438\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0432\u043E\u0439 \u0441\u043E\
  \u0431\u0441\u0442\u0432\u0435\u043D\u043D\u044B\u0439, \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u044F \u043C\u0435\u0442\u043E\u0434 `to_ascii_uppercase`\
  \ \u0434\u043B\u044F \u043E\u0442\u0434\u0435\u043B\u044C\u043D\u044B\u0445 \u0441\
  \u0438\u043C\u0432\u043E\u043B\u043E\u0432 \u0438 \u043F\u0440\u043E\u0439\u0434\
  \u044F\u0441\u044C \u0446\u0438\u043A\u043B\u043E\u043C \u043F\u043E \u0441\u043B\
  \u043E\u0432\u0430\u043C."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как это сделать:
В Rust нет встроенного метода для преобразования каждого слова строки в заглавные буквы, но мы можем легко реализовать свой собственный, используя метод `to_ascii_uppercase` для отдельных символов и пройдясь циклом по словам.

```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let sentence = "hello world";
    println!("{}", capitalize_words(sentence));
}
```

Пример вывода:

```
Hello World
```

## Подробнее:
Исторически Rust придавал значение минимализму стандартной библиотеки, с многими вспомогательными функциями, предоставляемыми сообществом через крейты. Для преобразования регистра строк вы можете использовать крейт `heck` для более продвинутых преобразований регистра, таких как CamelCase, snake_case и других.

Преобразование строки в заглавные буквы может быть сложным с символами юникода. Тип `char` в Rust представляет собой скалярное значение Unicode, что позволяет корректно обрабатывать большинство символов. При работе с полной нормализацией Unicode следует рассмотреть использование более продвинутых библиотек, таких как `unicode-segmentation`, для операций, учитывающих кластеры графем.

С точки зрения реализации, наша функция `capitalize_words` не является высокопроизводительной, так как она выделяет новую строку для каждого слова. В приложениях, требующих высокой производительности, было бы полезно оптимизировать манипуляции со строками, чтобы избежать чрезмерных выделений памяти.

## Смотрите также:
- Документация Rust по 'char': https://doc.rust-lang.org/std/primitive.char.html
- Крейт 'Heck' для преобразований регистра: https://crates.io/crates/heck
- 'Формы нормализации Unicode' в Rust: https://unicode-rs.github.io/unicode-normalization/unicode_normalization/index.html
- Книга Rust для дополнительной информации о строках: https://doc.rust-lang.org/book/ch08-02-strings.html
