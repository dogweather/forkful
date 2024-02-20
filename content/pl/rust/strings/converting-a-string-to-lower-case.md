---
date: 2024-01-20 17:39:16.691373-07:00
description: "Zmiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza przekszta\u0142\
  cenie wszystkich wielkich liter w danym ci\u0105gu na ich odpowiedniki w dolnym\
  \ rejestrowie.\u2026"
lastmod: 2024-02-19 22:04:54.297366
model: gpt-4-1106-preview
summary: "Zmiana \u0142a\u0144cucha znak\xF3w na ma\u0142e litery oznacza przekszta\u0142\
  cenie wszystkich wielkich liter w danym ci\u0105gu na ich odpowiedniki w dolnym\
  \ rejestrowie.\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zmiana łańcucha znaków na małe litery oznacza przekształcenie wszystkich wielkich liter w danym ciągu na ich odpowiedniki w dolnym rejestrowie. Programiści robią to dla jednolitości danych, łatwiejszego porównywania tekstów i spełnienia specyficznych wymagań aplikacji.

## How to: (Jak to zrobić:)
```Rust
fn main() {
    let original = "Witaj Świecie!";
    let lowercased = original.to_lowercase();
    println!("Oryginał: {}", original);
    println!("Małe litery: {}", lowercased);
}
```
Output:
```
Oryginał: Witaj Świecie!
Małe litery: witaj świecie!
```
## Deep Dive (Dogłębna analiza)
W Rust, metoda `.to_lowercase()` powstała z potrzeby uniwersalnego konwertera tekstu. Umożliwia precyzyjną zmianę na małe litery, nawet w przypadku skomplikowanych przypadków z Unicode. Alternatywą jest własnoręczne iterowanie po znakach i używanie metody `.to_ascii_lowercase()`, którą jednak warto stosować tylko gdy pracujemy z ASCII.

Unicode zawiera różne przypadki i wyjątki, dlatego też proces konwersji nie jest trywialny i wymaga precyzyjnych algorytmów. Implementacja `to_lowercase()` uwzględnia te subtelności. Na przykład, niemiecka litera 'ß' nie ma dużego odpowiednika i w konwersji na wielkie litery musi się zmienić w "SS", lecz w przypadku konwersji na małe litery pozostaje bez zmian.

## See Also (Zobacz również)
- [Rust Documentation for `to_lowercase`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Unicode Case Folding](https://www.unicode.org/reports/tr44/#CaseFolding)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
