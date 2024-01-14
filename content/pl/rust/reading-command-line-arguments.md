---
title:    "Rust: Odczytywanie argumentów wiersza poleceń"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Często podejmujemy decyzję o tym, czy nasz program powinien wykonywać różne akcje w zależności od argumentów podanych przy jego uruchomieniu. Dzięki umiejętności czytania argumentów wiersza poleceń, możemy stworzyć bardziej interaktywny i zindywidualizowany program dla użytkowników. W tym blogu dowiesz się, jak szybko i łatwo czytać argumenty wiersza poleceń w języku Rust.

## Jak To Zrobić

Aby czytać argumenty wiersza poleceń w języku Rust, wykonaj poniższe kroki:

1. Zaimportuj moduł `std::env`, dzięki któremu będziesz mógł uzyskać dostęp do argumentów wiersza poleceń.

```Rust
use std::env;
```

2. Wywołaj metodę `args()` na obiekcie `env`, aby uzyskać listę argumentów podanych przy uruchomieniu programu.

```Rust
let args: Vec<String> = env::args().collect();
```

3. W celu odczytania argumentów z listy, możemy użyć funkcji `nth()`, podając jej indeks argumentu, który chcemy odczytać.

```Rust
let first_arg = args.nth(0).expect("No argument given");
```

4. Aby wypisać odczytany argument, możemy użyć polecenia `println!()`.

```Rust
println!("First argument: {}", first_arg);
```

## Deep Dive

Metoda `args()` zwraca iterator, dzięki czemu możemy łatwo iterować przez wszystkie argumenty wiersza poleceń.

```Rust
for arg in env::args() {
    println!("Argument: {}", arg);
}
```

Należy jednak pamiętać, że pierwszym elementem listy jest nazwa programu, a nie pierwszy argument. Aby przeskoczyć do pierwszego argumentu, możemy wykorzystać metodę `skip()`.

```Rust
for arg in env::args().skip(1) {
    println!("Argument: {}", arg);
}
```

Jeśli chcemy odczytać argumenty jako liczby lub inne typy danych, możemy użyć odpowiednich metod, takich jak `parse()` czy `unwrap()`. Przykładowo, jeśli chcemy odczytać liczby całkowite jako `i32`, możemy użyć poniższego kodu.

```Rust
let int_arg: i32 = args.nth(0).expect("No argument given").parse().unwrap();
```

## See Also
- Dokumentacja Rust dotycząca modułu `std::env`: [https://doc.rust-lang.org/std/env/index.html](https://doc.rust-lang.org/std/env/index.html)
- Przykład użycia modułu `std::env` w projekcie na GitHubie: [https://github.com/rust-lang/rust/blob/master/src/libstd/env.rs](https://github.com/rust-lang/rust/blob/master/src/libstd/env.rs)
- Wszystkie dostępne metody w module `std::env`: [https://doc.rust-lang.org/std/env/fn.args.html](https://doc.rust-lang.org/std/env/fn.args.html)