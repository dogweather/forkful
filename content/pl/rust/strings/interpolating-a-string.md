---
date: 2024-01-20 17:51:43.107413-07:00
description: "Interpolacja string\xF3w pozwala wstawia\u0107 zmienne lub wyra\u017C\
  enia bezpo\u015Brednio do \u0142a\u0144cuch\xF3w znak\xF3w. Programi\u015Bci u\u017C\
  ywaj\u0105 jej dla czytelno\u015Bci i wygody, unikaj\u0105c\u2026"
lastmod: '2024-03-13T22:44:35.169948-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w pozwala wstawia\u0107 zmienne lub wyra\u017Cenia\
  \ bezpo\u015Brednio do \u0142a\u0144cuch\xF3w znak\xF3w."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## How to: (Jak to zrobić:)
```rust
fn main() {
    let planet = "Earth";
    let population = 7_753_000_000; // liczba przybliżona

    // Prosty przykład interpolacji stringów z makrem format!
    let greeting = format!("Witaj na planecie o nazwie: {}, która ma około {} ludności.", planet, population);
    println!("{}", greeting);
}

// Output:
// Witaj na planecie o nazwie: Earth, która ma około 7753000000 ludności.
```

## Deep Dive (Dogłębna analiza)
Interpolacja stringów w Rust jest realizowana przez szereg makr, głównie: `format!`, `print!` i `println!`, gdzie `format!` zwraca `String`, a pozostałe wypisują tekst na konsolę. Odzwierciedla to podejście języka do bezpieczeństwa typów i zarządzania pamięcią, unikając niejawnych konwersji.

W przeszłości, w innych językach programowania jak Perl czy PHP, interpolacja była często wbudowana bezpośrednio w składnię stringów. Rust, dążąc do wyraźnego oddzielenia danych od kodu, wymaga stosowania makr.

Jedną z alternatyw jest użycie biblioteki "writeln!" dla plików lub "format_args!" - niskopoziomowego makra wydajnościowego, które nie alokuje pamięci przez co jest szybsze. Pamiętaj jednak, że zwiększa to złożoność kodu.

Implementacja makr `format!`, `print!`, i `println!` polega na parserze w czasie kompilacji, który interpola stringi, zamieniając zmienne i wyrażenia na odpowiednie miejsce w docelowym stringu.

## See Also (Zobacz również)
- Rust Book: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html) - rozdział o stringach
- The Rust Standard Library Documentation: [https://doc.rust-lang.org/std/macro.format.html](https://doc.rust-lang.org/std/macro.format.html) - dokumentacja makra format!
