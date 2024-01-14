---
title:    "Rust: Wyświetlanie danych debugowania"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie komunikatów debugowania jest nieodłączną częścią programowania. Pomaga nam zrozumieć, co dzieje się w naszym kodzie i znaleźć ewentualne błędy. W języku Rust, wyświetlanie tych informacji jest szczególnie przydatne ze względu na jego bezpieczeństwo i wydajność. Dlatego warto poznać jak korzystać z drukowania komunikatów debugowania w Rust.

## Jak to zrobić

Najprostszym sposobem na wyświetlanie komunikatów debugowania w Rust jest użycie funkcji `println!()`. Przykładowe użycie wygląda tak:

```Rust
let num = 10;
println!("Wartość zmiennej num to: {}", num);
```

Kod ten wyświetli w konsoli tekst "Wartość zmiennej num to: 10". Zauważmy, że podwójny ukośnik w nawiasie funkcji `println!()` służy do wstawienia wartości zmiennej w odpowiednie miejsce w tekście. W przypadku użycia typu złożonego, np. struktury lub krotki, musimy dodać kolejne symbole ukośnika i podać odpowiednie zmienne.

Możemy także wykorzystać funkcję `dbg!()` do wyświetlania informacji o wartości danej zmiennej. W tym przypadku nie musimy podawać tekstu do wyświetlenia, po prostu przekazujemy zmienną jako argument funkcji `dbg!()`.

```Rust
let name = "Anna";
dbg!(name);
```

Wyświetli nam się w konsoli informacja o wartości zmiennej `name` oraz jej typie, czyli "name = "Anna" : &str".

## Deep Dive

W języku Rust możemy także wykorzystać makra, aby wyświetlać komunikaty debugowania. Makra są wywoływane za pomocą wykrzyknika przed nazwą np. `println!()` jest makrem. Makra pozwalają nam na większą kontrolę nad wyświetlanym tekstem, ponieważ możemy wykorzystać operatory i warunki, dzięki czemu możemy wyświetlać tylko wybrane informacje w zależności od danego warunku.

Ponadto, w Rust możemy także wykorzystać bibliotekę `log` do wyświetlania komunikatów debugowania. Dzięki niej możemy ustalić poziom ważności wyświetlanych informacji i w zależności od tego, czy pracujemy na lokalnym środowisku czy wdrożeniem, wyświetlać tylko wybrane komunikaty.

## Zobacz też

- [Dokumentacja Rust - Debugowanie](https://doc.rust-lang.org/rust-by-example/println/print_debug.html)
- [Tutorial Rust - Komunikaty debugowania](https://www.tutorialspoint.com/rust/rust_debugging.htm)
- [Strona oficjalna biblioteki log](https://docs.rs/log/0.4.0/log/)