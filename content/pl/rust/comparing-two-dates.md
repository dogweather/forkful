---
title:    "Rust: Porównywanie dwóch dat"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego porównywać daty?

Porównywanie dat jest nieodłącznym elementem programowania, niezależnie od wybranego języka. W przypadku Rusta, porównywanie dat może być szczególnie przydatne w sytuacjach, gdy potrzebujemy ustalić, która z dwóch dat jest wcześniejsza lub późniejsza. W tym artykule dowiesz się, jak porównywać daty w języku Rust oraz jak przeprowadzić bardziej szczegółową analizę tego tematu.

## Jak to zrobić?

W Rust, porównywanie dat jest możliwe dzięki modułowi `chrono`, który dostarcza wiele przydatnych funkcji do pracy z datami i czasem. Aby zacząć, musimy najpierw zaimportować ten moduł używając dyrektywy `use`, jak poniżej:

```Rust
use chrono::{Utc, DateTime};
```

Następnie należy zadeklarować dwie zmienne typu `DateTime`, które będą reprezentować daty, które chcemy porównać:

```Rust
let first_date = Utc::now(); // bieżąca data i czas w formacie UTC
let second_date = DateTime::parse_from_rfc3339("2021-08-20T12:00:00+00:00").unwrap(); // konwersja daty z formatu RFC 3339
```

W powyższym przykładzie użyliśmy funkcji`now()` do pobrania aktualnego czasu, a także wykorzystaliśmy funkcję `parse_from_rfc3339()` do przekonwertowania daty w formacie RFC 3339 na typ `DateTime`. Teraz możemy porównać te dwie daty przy użyciu operatorów porównania takich jak `<`, `>`, `<=`, `>=` lub funkcji `eq()` i `ne()`.

```Rust
println!("Is first date earlier than second date? {}", first_date < second_date);
```

Wynik powyższego kodu jest zależny od aktualnej daty i czasu, ale możemy też dokładnie podać daty, które chcemy porównać.

## Wnikliwiej o porównywaniu dat

W Rust, można również stosować funkcje `partial_cmp()` oraz `cmp()` do porównywania dat. Dzięki temu możemy uzyskać informację o wzajemnym położeniu dwóch dat oraz dokładną wartość w przypadku, gdy obie daty są równe.

```Rust
let comparison = first_date.partial_cmp(second_date).unwrap();

if comparison == Ordering::Less {
    println!("First date is earlier than second date");
} else if comparison == Ordering::Greater {
    println!("Second date is earlier than first date");
} else {
    println!("Both dates are equal");
}
```

Funkcja `partial_cmp()` zwraca typ `Option<Ordering>` - wartość `Some(Ordering)` gdy daty są różne oraz `None` gdy są one równe. Dzięki temu unikamy potencjalnych błędów związanych z porównywaniem dat.

## Zobacz również

- [Dokumentacja modułu chrono](https://docs.rs/chrono/)
- [Porównywanie dat w języku Rust](https://www.internalpointers.com/post/compare-dates-rust)
- [Struktura DateTime w bibliotece chrono](https://rust-datetime.github.io/chrono/chrono/struct.DateTime.html)