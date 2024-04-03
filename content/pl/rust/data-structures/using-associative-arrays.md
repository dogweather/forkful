---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:53.670203-07:00
description: "Jak to zrobi\u0107: W Rust, typ `HashMap` z modu\u0142u `std::collections`\
  \ zapewnia funkcjonalno\u015B\u0107 tablic asocjacyjnych. Oto jak mo\u017Cesz z\
  \ nich korzysta\u0107."
lastmod: '2024-03-13T22:44:35.177166-06:00'
model: gpt-4-0125-preview
summary: "W Rust, typ `HashMap` z modu\u0142u `std::collections` zapewnia funkcjonalno\u015B\
  \u0107 tablic asocjacyjnych."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

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
