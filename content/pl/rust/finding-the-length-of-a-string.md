---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## Co i dlaczego?

Wyszukiwanie długości ciągu w języku programowania odnosi się do określenia liczby znaków w tym ciągu. Robimy to, aby wiedzieć, jak dużo miejsca zajmuje ciąg w pamięci i jak efektywnie możemy nim manipulować.

## Jak to zrobić:

Oto podstawowy przykład, jak to zrobić w Rust:

```Rust
fn main() {
    let napis = "Witaj, Świecie!";
    println!("Długość napisu to: {}", napis.len());
}
```
Kiedy uruchomimy ten program, otrzymamy następujący wynik:

```Rust
Długość napisu to: 15
```
## Głębszy wgląd

Rust, jak wiele innych języków programowania, traktuje ciągi jako sekwencje bajtów, nie znaków. Dlatego funkcja `len()` zwraca liczbę bajtów, a nie znaków w ciągu. Ważne jest to zrozumienie, szczególnie przy pracy z ciągami zawierającymi znaki Unicode.

Opcją jest użycie metody `chars().count()` do policzenia faktycznej liczby znaków, ale zazwyczaj jest to wolniejsze niż użycie `len()`.

```Rust
fn main() {
    let napis = "Witaj, Świecie!";
    println!("Liczba znaków w ciągu to: {}", napis.chars().count());
}
```
Możemy na przykład używać tej metody, gdy pracujemy z ciągami zawierającymi wielobajtowe znaki Unicode.

## Zobacz także:

- Dokumentacja Rust dla `str.len`: https://doc.rust-lang.org/std/primitive.str.html#method.len
- Dokumentacja Rust dla `str.chars`: https://doc.rust-lang.org/std/primitive.str.html#method.chars

Ważne jest, aby zrozumieć różnicę między liczbą bajtów a liczbą znaków w ciągu, zwłaszcza przy pracy z różnymi zestawami znaków i kodowaniem.