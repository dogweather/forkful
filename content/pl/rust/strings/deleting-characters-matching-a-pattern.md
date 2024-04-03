---
date: 2024-01-20 17:43:21.325677-07:00
description: "How to (Jak to zrobi\u0107): W Rust u\u017Cywamy biblioteki standardowej\
  \ lub zewn\u0119trznych crate\u2019\xF3w (pakiet\xF3w) do manipulacji ci\u0105gami\
  \ znak\xF3w. Oto przyk\u0142ad, jak\u2026"
lastmod: '2024-03-13T22:44:35.167628-06:00'
model: gpt-4-1106-preview
summary: "W Rust u\u017Cywamy biblioteki standardowej lub zewn\u0119trznych crate\u2019\
  \xF3w (pakiet\xF3w) do manipulacji ci\u0105gami znak\xF3w."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to (Jak to zrobić):
W Rust używamy biblioteki standardowej lub zewnętrznych crate’ów (pakietów) do manipulacji ciągami znaków. Oto przykład, jak możemy usunąć wszystkie wystąpienia litery "a" z ciągu znaków:

```Rust
fn main() {
    let example_string = "bananas in pyjamas";
    let filtered_string: String = example_string.chars()
        .filter(|&c| c != 'a')
        .collect();
    println!("Filtered string: {}", filtered_string);
}
```

Wynik będzie wyglądać tak:
```
Filtered string: bnn s n pyjms
```

Możemy również użyć wyrażeń regularnych (regex), by obsłużyć bardziej złożone wzorce:

```Rust
use regex::Regex;

fn main() {
    let regex = Regex::new("a").unwrap();
    let example_string = "bananas in pyjamas";
    let result = regex.replace_all(&example_string, "");
    println!("Filtered string: {}", result);
}
```

Wynik pozostaje taki sam:
```
Filtered string: bnn s n pyjms
```

## Deep Dive (Dogłębna analiza):
Usuwanie znaków według wzorca nie jest nowością; mechanizmy takie jak wyrażenia regularne są w użyciu od dekad. W Rust, wydajność jest kluczowa, więc wbudowane metody jak `filter` są zoptymalizowane pod tym kątem. Alternatywą dla wyrażeń regularnych może być używanie metody `replace` lub `replacen` dla prostych przypadków.

Implementacja `filter` wykorzystuje iterator, który leniwie przetwarza elementy, co jest efektywne dla dużych danych. Regex, dostępne dzięki crate'owi `regex`, jest potężniejsze, ale może wprowadzić dodatkowe koszty wydajnościowe w przypadku prostych operacji.

## See Also (Zobacz również):
- Dokumentacja Rust dla [`str::replace`](https://doc.rust-lang.org/std/primitive.str.html#method.replace)
- Tutorial Rust do wyrażeń regularnych [Rust Regex Tutorial](https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html)
