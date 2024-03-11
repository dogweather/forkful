---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:53.670203-07:00
description: "Tablice asocjacyjne, czyli to, co Rustaceanie nazywaj\u0105 \"mapami\
  \ hash\", to kolekcje przechowuj\u0105ce dane w parach klucz-warto\u015B\u0107.\
  \ Programi\u015Bci u\u017Cywaj\u0105 ich do\u2026"
lastmod: '2024-03-11T00:14:08.350424-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne, czyli to, co Rustaceanie nazywaj\u0105 \"mapami hash\"\
  , to kolekcje przechowuj\u0105ce dane w parach klucz-warto\u015B\u0107. Programi\u015B\
  ci u\u017Cywaj\u0105 ich do\u2026"
title: Korzystanie z tablic asocjacyjnych
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, czyli to, co Rustaceanie nazywają "mapami hash", to kolekcje przechowujące dane w parach klucz-wartość. Programiści używają ich do szybkiego wyszukiwania danych, co pozwala na efektywną manipulację danych na podstawie unikalnych kluczy.

## Jak to zrobić:

W Rust, typ `HashMap` z modułu `std::collections` zapewnia funkcjonalność tablic asocjacyjnych. Oto jak możesz z nich korzystać:

```Rust
use std::collections::HashMap;

fn main() {
    // Tworzenie nowego HashMap
    let mut wyniki = HashMap::new();

    // Wstawianie wartości
    wyniki.insert(String::from("Niebieski"), 10);
    wyniki.insert(String::from("Żółty"), 50);

    // Dostęp do wartości
    let nazwa_druzyny = String::from("Niebieski");
    if let Some(wynik) = wyniki.get(&nazwa_druzyny) {
        println!("Wynik dla drużyny Niebieski: {}", wynik); // Wyjście: Wynik dla drużyny Niebieski: 10
    }

    // Aktualizacja wartości
    wyniki.entry(String::from("Niebieski")).and_modify(|e| *e += 5);

    // Iterowanie przez pary klucz-wartość
    for (klucz, wartosc) in &wyniki {
        println!("{}: {}", klucz, wartosc); // Wyjście: Niebieski: 15, Żółty: 50
    }
}
```

## Wgłębienie

`HashMap` w Rust używa funkcji hashującej do mapowania kluczy na wartości, co umożliwia szybkie odzyskiwanie danych. Jednak ta skuteczność wiąże się z kosztem: mapy hash nie zachowują kolejności swoich elementów. Jest to w przeciwieństwie do innych implementacji tablic asocjacyjnych, jak te w Pythonie (`dict`) czy Ruby, które w ostatnich wersjach utrzymują kolejność wstawiania jako funkcję. W przypadkach, gdy kolejność par klucz-wartość jest znacząca, developerzy Rust mogą rozważyć użycie `BTreeMap` z modułu `std::collections`, który zachowuje kolejność, ale może oferować wolniejsze wstawianie i pobieranie w porównaniu do `HashMap`. Ostatecznie wybór pomiędzy `HashMap` a `BTreeMap` zależy od konkretnych wymagań dotyczących kolejności i wydajności.
