---
title:    "Rust: Łączenie ciągów znaków"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Konkatenacja stringów jest nieodłącznym elementem programowania w języku Rust. Dzięki niej możemy łączyć różne ciągi znaków ze sobą, co jest niezbędne w wielu zastosowaniach. W tym artykule dowiesz się, dlaczego warto poznać tę funkcję oraz jak ją wykorzystać w praktyce.

## Jak to zrobić

Aby skorzystać z konkatenacji stringów w języku Rust, wystarczy użyć operatora `+` i połączyć ze sobą dwa lub więcej ciągów znaków.

```Rust
let str1 = "Witaj";
let str2 = "świecie!";

let result = str1 + " " + str2;
// Wynik: "Witaj świecie!"
```

Możemy także łączyć zmienne typu `String` z zwykłymi ciągami znaków oraz z innymi zmiennymi typu `String`.

```Rust
let str = String::from("Rust");
let msg = "Język " + str + " jest bardzo wydajny.";

// Wynik: "Język Rust jest bardzo wydajny."
```

Konkatenacja może odbywać się także poprzez metodę `format!`, która pozwala na bardziej zaawansowane manipulacje ze zmiennymi i formatowaniem tekstu.

```Rust
let name = "Anna";
let age = 28;
let info = format!("Nazywam się {} i mam {} lat.", name, age);

// Wynik: "Nazywam się Anna i mam 28 lat."
```

## Dogłębne przeglądanie tematu

Funkcja konkatenacji stringów może być wykorzystywana na wiele różnych sposobów. Możemy jej używać do łączenia tekstu z zmiennymi, tworzenia wyrażeń oraz generowania dynamicznych komunikatów. Warto także pamiętać o obsłudze błędów związanych z nieprawidłowym łączeniem danych różnych typów.

## Zobacz także

- [Dokumentacja Rust na temat konkatenacji stringów](https://doc.rust-lang.org/std/string/struct.String.html#method.add)
- [Artykuł o manipulacji stringami w Rust na blogu Mozilla](https://blog.mozilla.org/lthomson/2015/05/21/string-manipulation-in-rust/)
- [Poradnik na temat pracy ze zmiennymi typu String w Rust](https://www.tutorialspoint.com/rust/rust_string_manipulation.htm)