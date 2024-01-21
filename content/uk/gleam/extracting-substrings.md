---
title:                "Виділення підрядків"
date:                  2024-01-20T17:46:00.711459-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке виділення підрядків та навіщо це програмістам?
Extracting substrings means pulling out specific parts of a string. Programmers do it to isolate, analyze, or manipulate specific data within a larger text.

## How to:
Gleam має вбудовані функції для цього:

```gleam
import gleam/string

pub fn main() {
  let text = "Привіт, світ!"
  let hello = string.slice(text, 0, 6) // "Привіт"
  let world = string.slice(text, 8, 13) // "світ"

  // Alternative method:
  let (hello_alt, _) = string.split_at(text, 7) // ("Привіт,", "світ!")
  let (_, world_alt) = string.split_at(hello_alt, 8) // ("Привіт,", "світ")
  
  // Вивід
  hello
  world
  hello_alt
  world_alt
}
```

## Deep Dive
Виділення підрядків з'явилося щойно з'явилися рядки. В інших мовах є схожі функції: Python має срізи, в JavaScript — `substring`, в Rust — `slice`.

В Gleam, `string.slice` дозволяє вирізати підрядок за індексами. Альтернативно `string.split_at` ділить рядок на дві частини: до і після зазначеного індексу, дозволяючи обрати потрібну частину.

Історично, виділення підрядків — це операція над масивами символів. У більшості мов програмування рядки імплементуються як масиви символів, а виділення підрядка включає копіювання підмасиву в нове місце в пам’яті. 

## See Also
- Gleam String module docs: https://hexdocs.pm/gleam_stdlib/gleam/string/
- A String processing guide: https://learnmeabitaboutgleam.com/chapter/string_fun/
- Efficient string handling in Gleam: https://gleam.run/book/tour/strings.html