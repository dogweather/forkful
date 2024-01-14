---
title:    "Rust: Pisanie do standardowego błędu"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu, czyli strumienia danych, które są błędne lub niepoprawne, jest nieodłączną częścią programowania w języku Rust. Bardzo ważne jest, aby wyświetlić odpowiednie komunikaty błędów, aby ułatwić użytkownikom zrozumienie co się stało i jak mogą to naprawić. W artykule tym omówimy dlaczego pisanie do standardowego błędu jest tak ważne oraz jak to zrobić w języku Rust.

## Jak to zrobić

Aby pisać do standardowego błędu w języku Rust, należy użyć makra "eprintln!". Poniżej przedstawiamy prosty przykład, w którym próbujemy otworzyć plik, ale w przypadku niepowodzenia zostanie wypisany odpowiedni komunikat błędu:

```Rust
use std::fs::File;

fn main() {
    let file = File::open("nieistniejacy_plik.txt").unwrap_or_else(|error| {
        eprintln!("Nie można otworzyć pliku: {}", error);
        std::process::exit(1);
    });
}
```

W tym przykładzie wykorzystujemy metode "unwrap_or_else()" do obsługi błędu i wypisania komunikatu z użyciem makra "eprintln!". Dzięki temu, użytkownik będzie wiedział dlaczego wystąpił błąd i jak go naprawić.

## Deep Dive

Istnieje również możliwość użycia funkcji "eprint!" do wypisywania komunikatów bez dodania nowej linii. Jest to szczególnie przydatne, gdy chcemy wypisać kilka informacji na temat błędu bez przejścia do nowej linii. Innym ważnym aspektem jest możliwość wypisywania do standardowego błędu nie tylko tekstów, ale także zmiennych i wartości. Poniżej przedstawiamy przykład, w którym chcemy przypisać wartość wprowadzoną przez użytkownika do zmiennej i wypisać ją w przypadku gdy jest niepoprawna:

```Rust
use std::io;

fn main() {
    let mut input = String::new();
    println!("Podaj liczbę: ");

    match io::stdin().read_line(&mut input) {
        Ok(_) => {
            let parsed_input: i32 = input.trim().parse().unwrap();
            if parsed_input < 0 {
                eprintln!("Podana liczba jest mniejsza od 0");
                std::process::exit(1);
            }
        }
        Err(error) => {
            eprintln!("Nie udało się odczytać liczby: {}", error);
            std::process::exit(1);
        }
    };
}
```

W tym przykładzie używamy metody "parse()" do przekształcenia wprowadzonej przez użytkownika wartości na typ "i32". W przypadku błędu, wypisujemy odpowiedni komunikat z użyciem makra "eprintln!".

## Zobacz również

- [Dokumentacja języka Rust - std::io::Error](https://doc.rust-lang.org/std/io/struct.Error.html)
- [Poradnik o debugowaniu w języku Rust](https://blog.thoughtram.io/rust/2016/03/14/rusts-panic-abort-and-unwind.html)
- [Poradnik o obsłudze błędów w języku Rust](https://blog.logrocket.com/a-practical-guide-to-handling-errors-in-rust/)

Dzięki znajomości tego prostego sposobu na pisanie do standardowego błędu w języku Rust, będziecie w stanie lepiej zarządzać i obsługiwać błędy w swoich programach. Pamiętajcie, że komunikaty błędów są bardzo ważne dla użytkowników, dlatego warto poświęcić czas na ich odpowiednie przygotowanie. Zaczynając przygodę z Rustem, warto zapoznać się z możliwoś