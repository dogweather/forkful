---
title:                "Rust: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливою частиною багатьох програм, оскільки вона дозволяє перевірити наявність необхідних файлів та зберегти час і зусилля користувачів.

## Як

```Rust
use std::fs;

fn main() {
    let directory = "my_directory";
    let result = fs::metadata(directory); // перевірка метаданих для директорії
    match result {
        Ok(_) => println!("Директорія існує"), // якщо директорія існує
        Err(_) => println!("Директорія не існує"), // якщо директорія не існує
    }
}
```

Приклад виходу:

```
Директорія існує
```

## Deep Dive

У Rust є кілька способів перевірити наявність директорії. Найпростіший спосіб - використання функції `fs::metadata()` з модуля `fs`. Ця функція повертає `std::io::Result<fs::Metadata>`, що містить інформацію про директорію, включаючи дату та час створення, розмір та дозволи на доступ. Обробка цього результату, як показано в прикладі вище, дозволяє вивести повідомлення про існування чи неіснування директорії.

## See Also

- [Документація зі стандартної бібліотеки Rust](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Блок-схема контролю перевірки директорії у Rust](https://app.diagrams.net/?lightbox=1&highlight=0000ff&edit=_blank&layers=1&nav=1&title=directory_existence_checker.drawio#R7VtbS6KwjGbT7I%2F7owo3dHD2qcRQcba5GkkMjaToHDYZBg6cU1GpbuoDNWuVtpsQ2xWItJcnxQiYuRN88TnVwoh%2Be8C2NhKDkDXeH8r2ecGYuNLNj0x6fLGvBPHVsO91iS9010jXSZNIyA1N6a7ys2leNjSQZUS8nkwIIsTqJ0oTtHgdOSG%2FjodBLoA0o8gRC9PGDnsSqWHE3M110mwkvAGR8Fh1B%2F5c%2FhFZDMaZxS5Hz2CseSVzf8FtKkGCpckOKOgQeR4Md84D1f%2BYKnrTPi4cyZJpJXqVfPpMgt7N1MQjUh0TH2ut72R%2BYBXWeGVj6PUbA1dgACJJxxQgPvPx5Oq%2B%2FQhTM5Nltp0pSlLUJZfP9NoDTVm5c8q2wFSzehhsyOh4BsxHegpG9wbw0EE35sP9pj%2FnRj%2BqzJsp%2BOhSZAeCbmOVDCO6FFvNYJKISezjfDHwKjZifH0CrWYZX5su4YBAdzZjWmTsCjmF5S9zcv3mTh0mxy%2FQ8%2BPLwGF62