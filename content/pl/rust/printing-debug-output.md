---
title:                "Rust: Wyświetlanie informacji debugujących"
simple_title:         "Wyświetlanie informacji debugujących"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu często jesteśmy zmuszeni do szukania błędów i debugowania kodu. W takich sytuacjach nieocenionym narzędziem jest wypisywanie informacji na temat działania programu w celu łatwiejszego zlokalizowania problemu. W języku Rust możemy to zrobić za pomocą funkcji `println!()` lub `eprintln!()`.

## Jak To Zrobić

Aby wypisać debugowe informacje w języku Rust, używamy funkcji `println!()` lub `eprintln!()`. Pierwsza z nich wypisuje informacje na standardowe wyjście, natomiast druga na wyjście diagnostyczne, co jest szczególnie przydatne w przypadku testów. Poniżej przedstawiamy przykładowy kod wykorzystujący funkcję `println!()`:

```Rust
fn main() {
    let x = 5;
    println!("Wartość zmiennej x: {}", x);
}
```

W powyższym przykładzie widzimy, że w funkcji `println!()` możemy wykorzystać specjalne znaczniki, takie jak `{}` do wypisania wartości zmiennej `x`. Dzięki temu uzyskujemy czytelny output, który może nam pomóc w debugowaniu kodu.

## Deep Dive

W języku Rust mamy również możliwość użycia makr `dbg!()` i `dbg!()` do wypisywania debugowych informacji. Makra te działają podobnie jak funkcje `println!()` i `eprintln!()`, ale mają dodatkową funkcjonalność - wypisują również nazwę i typ zmiennej. Poniżej przykład kodu wykorzystującego makro `dbg!()`:

```Rust
fn main() {
    let name = "John";
    let age = 28;

    dbg!(name, age);
}
```

Powyższy przykład spowoduje wyświetlenie informacji o nazwie i typie zmiennych `name` i `age` na wyjściu diagnostycznym.

## Zobacz Ewentualnie

Jeśli chcesz dowiedzieć się więcej o drukowaniu debugowych informacji w języku Rust, polecamy zapoznać się z oficjalną dokumentacją lub z poniższymi artykułami:

- [Printing to the terminal in Rust](https://dev.to/rogertorres/printing-to-the-terminal-in-rust-2pl)
- [Debugging Rust with println!() and dbg!() macros](https://www.speedrun.com/blog/debugging-rust-with-println-and-dbg-macros)

Mamy nadzieję, że dzięki wykorzystaniu funkcji `println!()` i `eprintln!()` wypisywanie debugowych informacji w języku Rust będzie dla Ciebie prostsze i skuteczniejsze. Powodzenia z debugowaniem!