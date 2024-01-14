---
title:                "Rust: Łączenie ciągów znaków"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas pisania kodu w Rust, często zdarza się, że musimy łączyć ze sobą ciągi znaków (strings). Dzięki temu możemy tworzyć bardziej złożone i użyteczne aplikacje. W tym artykule dowiesz się, dlaczego jest to ważna umiejętność dla każdego programisty.

## Jak to zrobić?

Aby połączyć ze sobą dwa ciągi znaków w Rust, możemy skorzystać z metody `concat()`. Przykładowy kod wygląda następująco:

```Rust
let first_string = "Witaj, ";
let second_string = "świecie!";
let full_string = first_string.to_owned() + second_string;

println!("Zawsze pamiętaj o łączeniu ciągów znaków: {}", full_string);
```

Po uruchomieniu tego kodu, powinniśmy zobaczyć na ekranie napis `Witaj, świecie!`. Zauważmy, że używając `to_owned()`, mamy dostęp do pełnej wersji pierwszego ciągu znaków, a nie tylko jego wersji wskazującej.

## Głębsze zagadnienia

Istnieje również wiele innych sposobów na łączenie ciągów znaków w Rust. Na przykład, używając operatora `+` albo wykorzystując metodę `format!()`, która pozwala na bardziej złożone formatowanie tekstu. Warto poeksperymentować z różnymi możliwościami i wybrać tę, która najlepiej pasuje do danej sytuacji.

## Zobacz także

- Dokumentacja Rust na temat "Strings": https://doc.rust-lang.org/std/string/struct.String.html
- Przykłady użycia metody `concat()`: https://www.geeksforgeeks.org/rust-string-concatenation/
- Poradnik na temat formatowania tekstu w Rust: https://www.techiedelight.com/formatting-strings-rust/