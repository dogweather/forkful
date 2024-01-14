---
title:    "Rust: Pisanie do standardowego błędu"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Standardowe wyjście oraz błąd to ważne części każdego programu. Wiedza na temat tego, jak wykorzystać standardowe wyjście jest niezbędna, jeśli chcemy wyświetlać wyniki naszych programów. Ale dlaczego powinniśmy również zwracać uwagę na standardowe wyjście błędu (standard error)? W tym artykule dowiecie się dlaczego jest to ważne i jak to zrobić w języku Rust.

## Jak to zrobić

W języku Rust, wypisywanie do standardowego wyjścia błędu jest bardzo proste. Wypisanie tekstu do standardowego wyjścia błędu można osiągnąć za pomocą funkcji `eprintln!`. W poniższym przykładzie wykorzystujemy funkcję `eprintln!` aby wyświetlić błąd, jeśli użytkownik wpisze złe hasło.

```Rust
use std::io;

fn main() {
    println!("Witaj! Podaj hasło:");

    let mut password = String::new();

    io::stdin()
        .read_line(&mut password)
        .expect("Nie udało się odczytać hasła.");

    if password.trim() != "rust" {
        eprintln!("Złe hasło!");
    }
}
```

Output:
```
Witaj! Podaj hasło:
rust
Złe hasło!
```

Jak widać, wypisanie tekstu do standardowego wyjścia błędu jest bardzo proste w języku Rust. Dodatkowo, możemy użyć funkcji `eprint!` aby wypisać błąd bez dodawania nowej linii.

## Głębsza analiza

Zwracanie uwagi na standardowe wyjście błędu jest ważne z kilku powodów. Po pierwsze, pozwala nam wyświetlać informacje o błędach lub ważnych komunikatach dla użytkownika, które nie powinny być mieszane z normalnymi wynikami programu. Po drugie, w przypadku gdy nasz program jest uruchamiany z konsoli, możemy w prosty sposób przekierować wszystkie wiadomości błędów do pliku dziennika, co ułatwia debugowanie i znajdowanie problemów.

Pamiętaj, że funkcja `eprintln!` wypisuje treść do standardowego wyjścia błędu, które jest oddzielone od standardowego wyjścia (println!) i może być przekierowane niezależnie.

## Zobacz również
- [Dokumentacja standardowej biblioteki Rust: std::io](https://doc.rust-lang.org/std/io/index.html)
- [Artykuł: Obsługa błędów w języku Rust](https://pl.wikibooks.org/wiki/Rust/Obs%C5%82uga_b%C5%82%C4%99d%C3%B3w)
- [Poradnik: Debugowanie w języku Rust](https://www.puritanic.net/2018/11/debugowanie-aplikacji-w-jezyku-rust/)