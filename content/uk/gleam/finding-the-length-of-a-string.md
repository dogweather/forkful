---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:48:08.407416-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Дізнатися довжину рядка - це означає визначити кількість символів у ньому. Програмісти роблять це для валідації, обробки тексту, чи просто для отримання інформації про дані.

## Як це зробити:
У Gleam визначення довжини рядка прямолінійне. Розглянемо приклад:

```gleam
import gleam/string

fn main() {
  let my_string = "Привіт, Україно!"
  let length = string.len(my_string)
  io.debug(length) // Виведе: 17
}
```

Код використовує функцію `len` із модуля `string` для отримання довжини рядка.

## Поглиблений Розгляд
Традиційно, функція знаходження довжини рядка має бути швидкою і надійною, адже вона є базовою операцією. У деяких мовах, таких як C, довжина рядка визначається перебором символів до спеціального термінатора. Але в мовах з вищим рівнем абстракції, як Gleam, ця дія маскується за простою функцією.

Існують альтернативи для визначення довжини рядка, наприклад, збереження довжини у структурі даних рядка, що дозволяє отримувати довжину без обчислень. У Gleam, `len` використовує найбільш ефективний для Erlang VM підхід.

Важливе зауваження: коли рахуємо довжину в Unicode рядках, маємо справу із тим, що один символ може займати кілька байт. Gleam обробляє це коректно, використовуючи UTF-8 кодування, так що довжина, яку ви отримаєте, буде в числі графем, а не окремих байтів.

## Дивіться Також
- Gleam Documentation on Strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Erlang's Efficiency Guide on Strings: [http://erlang.org/doc/efficiency_guide/myths.html#strings](http://erlang.org/doc/efficiency_guide/myths.html#strings)

Ці ресурси допоможуть глибше зрозуміти роботу із рядками та їхню обробку у Gleam та батьківській Erlang VM.