---
title:                "Odczytywanie argumentów z wiersza poleceń"
html_title:           "Rust: Odczytywanie argumentów z wiersza poleceń"
simple_title:         "Odczytywanie argumentów z wiersza poleceń"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znajdowałeś się w sytuacji, gdzie musiałeś wprowadzić pewne dane do programu podczas jego uruchamiania? Na przykład, ustawić ścieżkę do pliku lub wybrać inną opcję? Jeśli tak, to wiesz, jakie to może być frustrujące. Ale nie martw się, w języku Rust istnieje sposób, aby czytać te dane bez konieczności ich wprowadzania w trakcie uruchamiania programu. W tym artykule dowiesz się, jak łatwo i szybko odczytać argumenty linii poleceń w Rust.

## Jak To Zrobić

### Przygotowanie projektu

Zanim rozpoczniemy kodowanie, musimy stworzyć projekt w języku Rust. W tym celu możemy użyć narzędzia Cargo, które jest wbudowane w Rust. W terminalu wpiszmy następującą komendę:

```rust
cargo new command-line-args
```

Spowoduje to utworzenie nowego projektu o nazwie "command-line-args". Potem przejdźmy do utworzonego folderu za pomocą komendy `cd command-line-args`.

### Odczytywanie argumentów

W języku Rust do odczytania argumentów wykorzystuje się strukturę `std::env::args`, która zwraca iterator zawierający wszystkie wprowadzone argumenty. Na przykład, jeśli użytkownik uruchomi program następującym poleceniem w terminalu:

```bash
./program nazwa_pliku.txt
```

To wywołanie zostanie przekonwertowane na wektor zawierający ciągi "program" oraz "nazwa_pliku.txt". Aby uzyskać dostęp do tych argumentów, możemy użyć metody `collect()` na zwróconym przez `std::env::args` iteratorem. Następnie wywołując metodę `join(" ")` na wektorze, otrzymamy jedną ciągłą linię argumentów, którą możemy wyświetlić w terminalu.

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Wprowadzone argumenty: {}", args.join(" "));
}
```

### Przetwarzanie argumentów

Często musimy przetworzyć wprowadzone argumenty w celu dostosowania działania naszego programu. W tym celu można wykorzystać funkcję `parse()` na każdym elemencie wektora, aby przekonwertować go na odpowiedni typ danych. Na przykład, jeśli chcemy, aby drugi argument był liczbą całkowitą, możemy użyć metody `parse()` w następujący sposób:

```rust
let second_arg: i32 = args[1].parse().expect("Nie udało się przetworzyć argumentu.");
```

W przypadku, gdy podany argument nie może zostać przekonwertowany na określony typ, zostanie zwrócony błąd. Dzięki temu możemy obsłużyć potencjalne błędy i poinformować użytkownika o nieprawidłowościach w wprowadzonych danych.

## Deep Dive

W języku Rust istnieje wiele dodatkowych funkcji i bibliotek, które umożliwiają zaawansowane przetwarzanie argumentów linii poleceń, takie jak np. `clap` czy `getopts`. Te narzędzia pozwalają na bardziej elastyczne zarządzanie argumentami i mogą okazać się bardzo pomocne w większych projektach. Warto zwrócić uwagę na ich możliwości i wykorzystać je w odpowiednich sytuacjach.

## Zobacz też

Jeśli chciałbyś się dowiedzieć więcej o języku Rust oraz jego bibliotekach i narzędziach, to zapraszam do zapoznania się z poniższymi linkami:

- [Rust Language](https://www.rust-lang