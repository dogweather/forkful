---
title:    "Rust: Odczytywanie argumentów wiersza poleceń"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Rust i chcesz nauczyć się, jak czytać argumenty wiersza poleceń, to właściwy artykuł dla Ciebie! Poznasz dokładnie dlaczego jest to ważne oraz jak to zrobić.

## Jak to zrobić

Aby czytać argumenty wiersza poleceń w Rust, wystarczy skorzystać z funkcji `args()` oraz `next()`. Oto przykładowy kod:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect(); 
    let first_arg = args.next(); 
    println!("Pierwszy argument to: {:?}", first_arg); 
}
```

Jeśli uruchomisz ten kod z argumentem w wierszu poleceń, na przykład `cargo run test`, to otrzymasz taki wynik:

```
Pierwszy argument to: Some("test")
```

Możesz także użyć pętli `for` do iteracji przez wszystkie argumenty:

```Rust
use std::env;

fn main() {
    for arg in env::args() {
        println!("Argument: {}", arg); 
    }
}
```

## Dogłębna analiza

Podczas czytania argumentów wiersza poleceń w Rust, warto zauważyć kilka ważnych rzeczy. Przede wszystkim, funkcja `args()` zwraca iterator. Oznacza to, że możemy wywołać metodę `next()` wielokrotnie, aż do momentu, gdy wszystkie argumenty zostaną przeczytane. Ponadto, warto zauważyć, że `args()` zwraca też nazwę programu jako pierwszy argument.

## Zobacz również

- [Dokumentacja Rust na temat czytania argumentów wiersza poleceń](https://doc.rust-lang.org/std/env/fn.args.html)
- [Przykłady użycia czytnika argumentów wiersza poleceń w praktyce](https://github.com/rust-lang-nursery/getopts#examples)