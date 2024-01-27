---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і для чого?
Запис текстового файлу – це процес збереження даних у формі тексту на диску. Програмісти це роблять, щоб зберегти результати, конфігурації та особливі дані для подальшого використання чи обміну.

## Як це зробити:
```gleam
import gleam/erlang.file.{write}

pub fn write_to_file(contents: String) -> Result(Nil, String) {
  let result = file.write("my_file.txt", contents)
  case result {
    Ok(_) -> Ok(Nil)
    Error(error) -> Error(error)
  }
}

fn main() {
  write_to_file("Вітаю читачів!").unwrap()
}
```
Після виконання коду файл `my_file.txt` міститиме текст `Вітаю читачів!`.

## Поглиблений розбір:
Історично, запис файлів відбивався на різних носіях: від перфокарт до сучасних SSD. У Gleam, також можна використовувати альтернативні способи: стандартні потоки чи бази даних. При записі текстового файлу важливо обробити помилки, що можуть виникнути через права доступа, дисків простір, або інші системні обмеження.

## Додатково:
- [Erlang's File Module](http://erlang.org/doc/man/file.html)
- [Working with Files in Erlang](https://learnyousomeerlang.com/for-a-few-lines-more)
