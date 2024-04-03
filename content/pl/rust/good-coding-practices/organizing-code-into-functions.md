---
date: 2024-01-26 01:16:15.506147-07:00
description: "Jak to zrobi\u0107: Wyobra\u017A sobie, \u017Ce masz kod, kt\xF3ry kilka\
  \ razy oblicza powierzchni\u0119 ko\u0142a. Zamiast powtarza\u0107 formu\u0142\u0119\
  , zamykasz j\u0105 w funkcji."
lastmod: '2024-03-13T22:44:35.190698-06:00'
model: gpt-4-0125-preview
summary: "Wyobra\u017A sobie, \u017Ce masz kod, kt\xF3ry kilka razy oblicza powierzchni\u0119\
  \ ko\u0142a."
title: Organizowanie kodu w funkcje
weight: 18
---

## Jak to zrobić:
Wyobraź sobie, że masz kod, który kilka razy oblicza powierzchnię koła. Zamiast powtarzać formułę, zamykasz ją w funkcji.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("Powierzchnia koła wynosi: {}", area);
}
```

Wynik:

```
Powierzchnia koła wynosi: 78.53981633974483
```

## Dogłębna analiza
Historycznie, funkcje pochodzą z matematyki, gdzie przyporządkowują wejścia do wyjść. W kodowaniu są obecne już od czasów asemblera, chociaż nazywaliśmy je „podprogramami”. Funkcje w Rust mogą zwracać wartości, a nawet inne funkcje dzięki funkcjom pierwszoklasowym i domknięciom.

Alternatywy? Kod wbudowany lub makra, ale są bałaganem dla skomplikowanej logiki. Obiekty z metodami to inny sposób organizowania funkcjonalności, inny niż samodzielne funkcje.

Implementacja w Rust jest dość prosta. Funkcje deklarują typy swoich parametrów i typ zwracany. Przyjęło się, że nazwy są pisane 'snake case'. Masz publiczne funkcje (`pub fn`) do użytku poza modułem i prywatne do użytku wewnętrznego. I Rust ma tę fajną funkcję, że nie potrzebujesz słowa kluczowego `return` dla ostatniego wyrażenia w funkcji.

## Zobacz też
Sprawdź te materiały po więcej informacji:
- Książka o języku programowania Rust: [Funkcje](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust przykładami na temat [Funkcji](https://doc.rust-lang.org/rust-by-example/fn.html)
