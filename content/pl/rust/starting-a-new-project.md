---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Rust: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiasz się, dlaczego warto rozpocząć nowy projekt w Rust? Jest wiele powodów, dla których warto zacząć pracę w tym języku. Po pierwsze, Rust jest niesamowitym narzędziem dla programistów, którzy cenią sobie wydajność i bezpieczeństwo swojego kodu. Oprócz tego, społeczność Rust jest bardzo aktywna i chętnie dzieli się wiedzą, co sprawia, że nauka tego języka jest szybka i przyjemna.

## Jak to zrobić

Rust jest językiem statycznie typowanym, co oznacza, że zmienne muszą być zdefiniowane z określonym typem i nie mogą zmienić swojego typu podczas działania programu. To zapewnia większą pewność, że kod działa zgodnie z naszymi oczekiwaniami. Przypomina to trochę C++, ale posiada również inne ciekawe funkcje, takie jak system typów zależny od wartości, który pozwala na bezpieczne operowanie na danych bez konieczności ręcznego sprawdzania ich poprawności. Zobaczmy to na przykładzie:

```rust
fn main() {
    // Tworzymy zmienną "x" typu i32 i nadajemy jej wartość
    let x: i32 = 5;

    // Tworzymy nową zmienną "y" i wykorzystujemy wcześniej
    // zdefiniowaną wartość "x" do obliczenia jej wartości
    let y: i32 = x + 10;

    println!("Wartość y: {}", y);
}

```

Wyjście: Wartość y: 15

Możemy również definiować własne struktury danych i implementować na nich metody:

```rust
// Definiujemy strukturę "Student" z polami "imie" i "nazwisko"
struct Student {
    imie: String,
    nazwisko: String,
}

// Tworzymy dla tej struktury metodę "przedstaw_sie"
// która wyświetli imię i nazwisko studenta
impl Student {
    fn przedstaw_sie(&self) {
        println!("Cześć, jestem {} {}", self.imie, self.nazwisko);
    }
}

fn main() {
    // Tworzymy nowego studenta i wywołujemy na nim metodę
    let student = Student {
        imie: String::from("Anna"),
        nazwisko: String::from("Kowalska"),
    };

    student.przedstaw_sie();
}

```

Wyjście: Cześć, jestem Anna Kowalska

## Deep Dive

Teraz, gdy już wiesz jak zacząć pracę w Rust, warto wiedzieć, że aby zainstalować i uruchomić język na swoim komputerze, musisz mieć dobrą znajomość terminala i niektórych podstawowych narzędzi programistycznych, takich jak kompilator Rusta - `rustc` oraz menadżer pakietów `cargo`. Możesz również skorzystać z wielu dostępnych online kursów i samouczków, aby lepiej poznać ten język i jego możliwości.

## Zobacz również

- [Oficjalna strona Rust](https://www.rust-lang.org)
- [Dokumentacja Rust](https://doc.rust-lang.org/stable/book)
- [The Rust Programming Language - kurs na Udemy](https://www.udemy.com/course/rust-lang/)