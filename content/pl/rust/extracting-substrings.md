---
title:    "Rust: Wydobywanie podłańcuchów"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego?

Extracting substrings jest ważną częścią wielu programów, ponieważ pozwala na wydajne i precyzyjne manipulowanie tekstami. Znajomość tej funkcji jest niezbędna podczas tworzenia aplikacji, które muszą przetwarzać duże ilości danych tekstowych.

## Jak to zrobić?

```Rust
// Importujemy bibliotekę standardową

use std::str;

// Przykładowy tekst

let text = "To jest przykładowy tekst";

// Wybieramy podciąg od 8. znaku do końca tekstu

let substring = &text[8..];

// Wyświetlamy wynik
    
println!("{}", substring);

// Output: przykładowy tekst
```

```Rust
// Podajemy indeks początkowy i końcowy

let text = "To jest przykładowy tekst";

let start = 8;
let end = 16;

// Wybieramy podciąg o podanych indeksach

let substring = &text[start..end];

// Wyświetlamy wynik
    
println!("{}", substring);

// Output: przykładowy
```

## Głębszy zanurzenie

Funkcja do wydobywania podciągów dostępna jest w bibliotece standardowej Rusta. Przyjmuje ona dwa argumenty - pierwszym jest tekst, z którego chcemy wybrać podciąg, a drugim jest zakres indeksów. Warto zauważyć, że indeksy są liczone od zera, a znaki poza zakresem zostaną zwyczajnie zignorowane. Możemy więc łatwo wybrać fragmenty tekstu według swoich potrzeb.

## Zobacz także:

- Dokumentacja biblioteki standardowej Rusta: https://doc.rust-lang.org/std/str/fn.slice.html
- Przykładowe zastosowanie funkcji do wydobywania podciągów w projekcie: https://github.com/rust-lang/rust/issues/30178
- Artykuł "7 przykładów na używanie funkcji slice w Rust": https://nick.groenen.me/posts/rust-slices/