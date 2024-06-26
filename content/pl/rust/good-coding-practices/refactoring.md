---
date: 2024-01-26 03:37:27.793593-07:00
description: "Jak to zrobi\u0107: Zrefaktoryzujmy prosty fragment kodu w Rust, aby\
  \ uczyni\u0107 go bardziej idiomatycznym i \u0142atwiejszym w utrzymaniu. Zaczynamy\
  \ od funkcji, kt\xF3ra\u2026"
lastmod: '2024-03-13T22:44:35.193724-06:00'
model: gpt-4-0125-preview
summary: "Zrefaktoryzujmy prosty fragment kodu w Rust, aby uczyni\u0107 go bardziej\
  \ idiomatycznym i \u0142atwiejszym w utrzymaniu."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Zrefaktoryzujmy prosty fragment kodu w Rust, aby uczynić go bardziej idiomatycznym i łatwiejszym w utrzymaniu. Zaczynamy od funkcji, która oblicza sumę wektora liczb całkowitych:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Suma to {}", sum(&numbers));
}
```

Wynik:
```
Suma to 15
```

Teraz zrefaktoryzujmy to, aby użyć bardziej idiomatycznego Rusta, wykorzystując iteratory i metodę `fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Suma to {}", sum(&numbers));
}
```

Bez zmian w wyniku - wciąż jest `15` - ale zrefaktoryzowana wersja jest czystsza i wykorzystuje mocne strony Rusta, takie jak pożyczanie i metody iteratorów.

## Dogłębna analiza
Refaktoryzacja ma swoje korzenie w społeczności Smalltalk i została spopularyzowana w świecie Java przez książkę Martina Fowlera "Refaktoryzacja: Ulepszanie struktury istniejącego kodu". Jej zasady są uniwersalne i mają zastosowanie również do Rusta, gdzie bezpieczeństwo i współbieżność są nadrzędne. Rust zachęca do pisania solidnego kodu, wyłapując problemy w czasie kompilacji, więc podczas refaktoryzacji, kompilator Rusta działa jak siatka bezpieczeństwa.

Alternatywami dla ręcznej refaktoryzacji są narzędzia automatyczne, takie jak 'rustfmt' do formatowania kodu i 'clippy' do lintowania, które mogą sugerować bardziej idiomatyczne sposoby pisania kodu. Jednak dogłębna refaktoryzacja często wymaga przemyślanego zrozumienia projektu kodu, czego te narzędzia nie mogą w pełni zautomatyzować.

W Rust refaktoryzacja może dotyczyć poprawy wykorzystania typów, efektywnego wykorzystania czasów życia, redukcji niepotrzebnych alokacji lub zastosowania wzorców współbieżności, takich jak użycie `Arc<Mutex<T>>`, gdy jest to konieczne. Powszechne jest również przejście z `unwrap()` na bardziej ekspresyjne obsługiwanie błędów z `Result<T, E>`.

## Zobacz również
Aby zagłębić się w refaktoryzację w Rust:

- Książka o Rust: https://doc.rust-lang.org/book/
- Rust przykładami: https://doc.rust-lang.org/rust-by-example/
- Clippy, narzędzie do lintowania Rusta: https://github.com/rust-lang/rust-clippy
- "Refaktoryzacja: Ulepszanie struktury istniejącego kodu" autorstwa Martina Fowlera: https://martinfowler.com/books/refactoring.html
