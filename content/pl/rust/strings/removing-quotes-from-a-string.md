---
date: 2024-01-26 03:42:41.754667-07:00
description: "Jak to zrobi\u0107: Czasami masz ci\u0105g znak\xF3w z mieszanimi cudzys\u0142\
  owami, tak jak tutaj."
lastmod: '2024-04-05T21:53:36.609899-06:00'
model: gpt-4-0125-preview
summary: "Czasami masz ci\u0105g znak\xF3w z mieszanimi cudzys\u0142owami, tak jak\
  \ tutaj."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hello, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Wynik: Hello, Rustaceans!
}
```

Czasami masz ciąg znaków z mieszanimi cudzysłowami, tak jak tutaj:

```Rust
fn main() {
    let mixed_quoted = "'Rust mówi: \"Cześć, Świecie!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Wynik: Rust mówi: "Cześć, Świecie!"
}
```

Tutaj usunięte zostają tylko najbardziej zewnętrzne pojedyncze cudzysłowy.

## Dogłębna analiza
Gdy usuwasz cudzysłowy z ciągu znaków, możesz się zastanawiać, dlaczego to nie jest po prostu proste `.replace("\"", "")`. Na początku, praca z tekstem była mniej ustandaryzowana, a różne systemy miały różne sposoby przechowywania i przesyłania tekstu, często z jakimś rodzajem 'sekwencji ucieczki' dla specjalnych znaków. Metoda `trim_matches` w Rust jest bardziej wszechstronna, pozwala określić wiele znaków do przycięcia, oraz czy przycinać od początku (prefiks), końca (sufiks) czy z obu stron ciągu znaków.

Oczywiście, są alternatywy. Regex to potęga manipulacji ciągami znaków, zdolna do dopasowania złożonych wzorców, i byłaby przesadą dla samego usuwania cudzysłowów. Biblioteki takie jak `trim_in_place` mogą oferować przycinanie w miejscu bez narzutu tworzenia nowego obiektu `String`, co może być pożądane dla aplikacji krytycznych pod względem wydajności.

W rzeczywistości, `trim_matches` faktycznie iteruje przez znaki ciągu od obu końców, sprawdzając je pod kątem dostarczonego wzorca, aż znajdzie znak, który się nie zgadza. Jest efektywny w tym, co robi, ale zawsze bądź świadomy, że pracuje z wartościami skalarnymi Unicode. Jeśli twój ciąg może zawierać wielobajtowe znaki Unicode, nie musisz się martwić, że zostaną one rozdzielone.

## Zobacz również
- Dokumentacja Rust na temat manipulacji ciągami znaków: https://doc.rust-lang.org/book/ch08-02-strings.html
- Crate `regex` dla złożonych wzorców: https://crates.io/crates/regex
- Rust przez przykład dla praktycznych scenariuszy kodowania: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
