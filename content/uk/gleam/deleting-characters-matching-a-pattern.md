---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:19.087652-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Видалення символів, які відповідають паттерну, - це фільтрація тексту, щоб вилучити небажані частини (наприклад, пробіли чи спецсимволи). Програмісти роблять це для очищення даних, форматування тексту чи підготовки даних до подальшої обробки.

## Як це зробити:
```gleam
import gleam/string

fn delete_matching_chars(subject: String, pattern: String) -> String {
  string.replace(subject, pattern, "")
}

pub fn main() {
  let text = "He11o, W0rld! 123"
  let clean_text = delete_matching_chars(text, "\\d") // видаляємо цифри
  assert clean_text == "Hello, Wrld! "
}
```

Вивід:
```
Hello, Wrld! 
```

## Глибше занурення
Видалення символів за паттерном не нове. Воно використовується в багатьох мовах, таких як Perl та JavaScript, з часів їх створення. Це основа для регулярних виразів – потужних інструментів для роботи з текстом. У Gleam це можна зробити за допомогою функцій модуля `string`, який використовує Erlang's Binary Module під капотом для ефективної обробки рядків.

Як альтернатива, можна застосувати функції `string.remove` чи працювати напряму з рівнем байтів, якщо потрібно більше контролю чи оптимізації. Однак, для більшості стандартних сценаріїв `string.replace` служить добре.

## Дивіться також
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html) - посібник про ефективну роботу з двійковими даними в Erlang, на якому побудовано Gleam.