---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# ## Що це & навіщо?
Видобування підрядків (substring) - це процес вилучення ділянки з більшого рядка. Зазвичай, програмісти цим користуються для обробки або аналізу конкретних частин даних, представлених рядками.

# ## Як це зробити:
```Gleam
import gleam/string

// Оригінальний рядок
let my_string = "Привіт, світ!"

// Видобування підрядка
let hello = string.slice(my_string, 0, 6)

assert hello == "Привіт"
```
У цьому прикладі ми витягаємо перші 6 символів з рядка my_string.

## Поглиблено:
Історично, в сфері програмування завжди було потребу в обробці і аналізі текстових даних. Завдяки видобуванню підрядків, ми можемо працювати із конкретними частинами тексту, не змінюючи оригінал.

Зазвичай, видобування підрядків реалізується через функції рівня мови, наприклад, як slice в Gleam. Але можливе й використання регулярних виразів, уточнених пошукових алгоритмів тощо.

Якщо говорити про деталі імплементації, Gleam використовує Erlang VM і Erlang рядки, тому висока продуктивність і надійність операцій із рядками гарантована.

## Див. також:
1. Документація Gleam про рядки: [https://hexdocs.pm/gleam_stdlib/gleam/string.html](https://hexdocs.pm/gleam_stdlib/gleam/string.html)
3. Як працювати з рядками в Erlang: [https://erlang.org/doc/man/erl_syntax.html](https://erlang.org/doc/man/erl_syntax.html)