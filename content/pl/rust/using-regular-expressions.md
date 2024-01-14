---
title:    "Rust: Używanie wyrażeń regularnych"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są niezwykle przydatnym narzędziem w programowaniu, pozwalającym na szybkie i skuteczne przetwarzanie tekstu. W Rust, popularnym języku programowania, również można wykorzystywać regularne wyrażenia, co ułatwia pracę z tekstami w programach.

## Jak to zrobić?

Aby używać regularnych wyrażeń w Rust, należy najpierw zaimportować moduł "regex". Następnie, przy użyciu funkcji "Regex::new(pattern)", możemy utworzyć wzorzec dla wyrażenia, które chcemy wyszukać. W przykładzie poniżej, szukamy wystąpienia słowa "Rust" w tekście:

```rust
use regex::Regex;

let re = Regex::new("Rust").unwrap(); //tworzymy wzorzec
let text = "Nauka nowego języka programowania jest wyzwaniem, ale Rust jest warty poświęcenia."; //tekst, w którym szukamy
let result = re.captures(text); //szukamy dopasowania wzorca w tekście
println!("{:?}", result); //wyświetlamy wynik
```

Wynik to: Some (Captures (["Rust"]). Jest to struktura, która zawiera dane o dopasowanym tekście. W przypadku, gdy nie znajdziemy dopasowania, wynik będzie równy "None". 

Możemy dodać również flagi do funkcji "Regex::new()", aby dopasowanie było bardziej elastyczne. Przykładowe flagi to: "i" - ignorowanie wielkości liter, "m" - dopasowanie do wielu linii. Przykład z użyciem flagi "i":

```rust
use regex::Regex;

let re = Regex::new(r"(?i)RUST").unwrap(); //również można użyć przedrostka "r" aby oznaczyć string jako "raw"
let text = "Rust jest zarówno szybki, jak i bezpieczny."; //tekst, w którym szukamy
let result = re.capturs(text); //szukamy dopasowania
println!("{:?}", result); //wynik: Some (Captures (["Rust"]));
```

## Głębsze zagadnienia

Używanie regularnych wyrażeń może być zawiłe i wymaga nauki, jednak warto poznać kilka dodatkowych funkcji, aby móc w pełni wykorzystać ich potencjał. 

Funkcja "captures()" zwraca strukturę "Captures" zawierającą wyrażenie dopasowane do wzorca. Jednak, jeśli chcemy uzyskać jedynie tekst dopasowany do konkretnych grup, używamy funkcji "name()" lub "get()". Przykład:

```rust
use regex::Regex;

let re = Regex::new(r"(?P<lang>[\w\s]+) jest zarówno szybki, jak i (bezpieczny)").unwrap();
let text = "Rust jest zarówno szybki, jak i bezpieczny.";
let result = re.captures(text).unwrap(); //używamy metody "unwrap()" aby pobrać wartość z opakowania
println!("{}", result.name("lang"));//wynik: Rust
println!("{}", result.get(1).unwrap());//wynik: Rust
println!("{}", result.get(2).unwrap());//wynik: bezpieczny
```

Inne przydatne metody to: "split()" - dzielenie tekstu na części na podstawie wzorca, "replace()" - zastępowanie tekstu na podstawie wzorca, "find()" - znajdywanie pierwszego dopasowania w tekście.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o regularnych wyrażeniach w Rust, polecam przeczytać oficjalną dokumentację języka oraz tutorial na stronie regexcrate. 

- [Oficjalna dokumentacja Rust](https://doc.rust-lang.org/book/ch09-00-02-guessing-game-tutorial.html)
- [Tutorial regex w Rust](https://docs.rs/