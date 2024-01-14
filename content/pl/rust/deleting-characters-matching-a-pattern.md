---
title:                "Rust: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Chcesz dowiedzieć się, dlaczego czasami jest konieczne usuwanie znaków pasujących do wzoru w programowaniu Rust? Ten blog jest dla ciebie!

## Dlaczego

Czasami w trakcie programowania może zajść potrzeba usunięcia określonych znaków z tekstu, które pasują do określonego wzoru. Może to mieć różne przyczyny, np. poprawa formatowania lub przetwarzanie danych. W tym wpisie dowiesz się, jak łatwo i sprawnie usuwać znaki pasujące do wzoru w języku Rust.

## Jak to zrobić

Sprawdzenie, czy dany znak pasuje do danego wzoru jest bardzo proste w języku Rust. Możesz skorzystać z funkcji `match`, która przeszukuje tekst pod kątem określonego wzoru i wykona odpowiednie akcje w zależności od wyniku.

```Rust

let text = "To jest tekst do przetworzenia";
// X jest znakiem, który chcemy usunąć
let pattern = 'X';

let mut result = String::new();

for c in text.chars() {
    match c {
        // Jeśli znak jest równy X, to nie dodajemy go do wynikowego tekstu
        'X' => continue,
        // W przeciwnym przypadku dodajemy go do wyniku
        _ => result.push(c),
    }
}

println!("{}", result); // Wypisze "To jest tek do przetworzenia"

```

W powyższym przykładzie wykorzystujemy pętlę `for` do iteracji po każdym znaku w tekście, a następnie sprawdzamy jego pasowanie do wzoru przy użyciu funkcji `match`. W przypadku, gdy pasuje do wzoru, używamy słowa kluczowego `continue`, które pomija dany znak i przechodzi do następnego. W przeciwnym przypadku, dodajemy go do zmiennej `result` przy pomocy metody `push()`. Na końcu wypisujemy wynikowy tekst.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej na temat usuwania znaków pasujących do wzoru w języku Rust, warto zapoznać się z następującymi funkcjami:

- `to_string()` - konwertuje dane na typ `String`
- `chars()` - zwraca iterator po poszczególnych znakach w tekście
- `enumerate()` - zwraca iterator po indeksach i wartościach w danej kolekcji
- `remove()` - usuwa znak o podanym indeksie z tekstu
- `drain()` - usuwa wszystkie znaki z tekstu, które pasują do wzoru

## Zobacz także

- [Dokumentacja języka Rust](https://doc.rust-lang.org/)
- [Kurs programowania w języku Rust](https://www.udemy.com/course/krasa9123-mastering-rust/)
- [Blog o programowaniu w języku Rust](https://blog.rust-lang.org/)