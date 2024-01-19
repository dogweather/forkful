---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Drukowanie informacji debugujących to sposób na wyświetlanie krok po kroku stanu programu podczas jego działania. Programiści robią to, aby łatwiej zidentyfikować i naprawić błędy w kodzie.

## Jak zrobić:

Proste drukowanie informacji debugujących w Rust jest tak proste jak użycie funkcji `println!` z formatem `{:?}`. 

```Rust
struct ProstyStruct {
    a: i32,
    b: f32,
}

fn main() {
    let s = ProstyStruct { a: 3, b: 4.0 };
    println!("{:?}", s);
}
```

Efekt będzie taki, gdzie mamy `ProstyStruct { a: 3, b: 4.0 }`.

## Głębsze Zanurzenie: 

1. Kontekst historyczny: Debugowanie zostało wprowadzone w latach 50-tych i od tamtego czasu ewoluowało, stając się integralną częścią programowania.

2. Alternatywy: W Rust, możemy również używać funkcji `dbg!` do drukowania informacji debugujących, które zwracają wartość dla dalszego użycia w kodzie.

3. Szczegóły implementacji: Wydruk debugujący z `{:?}` wykorzystuje trait `std::fmt::Debug` do formatowania wyjścia. Musi on być zaimplementowany dla typu struktury.

## Zobacz również: 

1. Dokumentacja Rust na temat Debugowania: https://doc.rust-lang.org/rust-by-example/std/marker/trait.debug.html
2. Dokumentacja Rust na temat funkcji println!: https://doc.rust-lang.org/std/macro.println.html
3. Szczegóły na temat Debug Trait: https://doc.rust-lang.org/std/fmt/fn.debug_struct.html
4. Przewodnik Rust o debugowaniu: https://rust-lang.github.io/rustc-guide/print.html