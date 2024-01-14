---
title:                "Rust: Odczytywanie argumentów wiersza poleceń"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego?

Czy kiedykolwiek zastanawiałeś się, jak programy wiersza poleceń są w stanie akceptować argumenty? Możesz być programistą, który chce zwiększyć swoją wiedzę na temat języka Rust lub po prostu ciekawą osobą, która chce wiedzieć więcej na temat tego procesu. W tym blogu zgłębimy sposoby czytania argumentów wiersza poleceń w języku Rust.

## Jak To Zrobić?

Pierwszym krokiem w czytaniu argumentów wiersza poleceń w języku Rust jest zaimportowanie modułu `std::env`. Następnie, możemy wykorzystać funkcję `args()` aby uzyskać iterator, który zawiera nasze argumenty przekazane z wiersza poleceń. Przykład kodu przedstawiony poniżej pokazuje, jak uzyskać dostęp do argumentów i wyświetlić je na ekranie:

```rust
use std::env; //importowanie modułu

fn main() {
    let args: Vec<String> = env::args().collect(); //tworzenie iteratora z argumentami
    println!("Przekazane argumenty: {:?}", args); //wyświetlenie argumentów na ekranie
}
```

Przykładowe wywołanie tego programu może wyglądać następująco: `./program_name argument1 argument2`. Wtedy, na ekranie pojawi się wydruk: `Przekazane argumenty: [./program_name, argument1, argument2]`.

## Głębsze Zagłębianie Się

Po uzyskaniu podstawowej wiedzy na temat czytania argumentów wiersza poleceń w języku Rust, możesz dalej zgłębiać temat poprzez naukę o funkcjonalnościach modułu `std::env` oraz metodach, które można wykorzystać do manipulowania argumentami. Możesz również zobaczyć przykładowy projekt na GitHubie lub zgłębić temat struktury argumentów przekazywanych w terminalu.

## Zobacz Również

- Dokumentacja modułu `std::env` w języku Rust (https://doc.rust-lang.org/std/env/)
- Przykładowy projekt czytający argumenty wiersza poleceń w języku Rust (https://github.com/username/project_name)
- Dokumentacja terminalu w systemie operacyjnym, aby dowiedzieć się więcej o argumentach wiersza poleceń (np. dla systemu Linux: `man bash`)