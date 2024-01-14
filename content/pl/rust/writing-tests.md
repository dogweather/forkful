---
title:                "Rust: Pisanie testów"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki testom możemy upewnić się, że nasz program działa zgodnie z oczekiwaniami i minimalizować ryzyko błędów w przyszłości. W tym wpisie dowiesz się dlaczego warto pisać testy i jak w prosty sposób zacząć je implementować w języku Rust.

## Jak To Zrobić

Zanim przejdziemy do tworzenia testów, musimy pamiętać o kilku rzeczach. Po pierwsze, nasze testy powinny być niezależne od siebie i niezależne od kodu aplikacji. Po drugie, należy pamiętać, że testy powinny być proste i łatwe do zrozumienia. W końcu, każdy test powinien sprawdzać tylko jedną funkcję lub metodę.

Pierwszym krokiem jest dodanie `#[cfg(test)]` przed naszą funkcją lub metodę, aby powiedzieć kompilatorowi, że jest to test. Następnie używamy makra `assert_eq!` aby porównać oczekiwany wynik z rzeczywistym. Patrząc na przykład, kod wyglądałby tak:

```Rust
#[cfg(test)]
fn zsumuj(a: i32, b: i32) -> i32 {
   a + b
}

#[cfg(test)]
mod testy {
   use super::*;

   #[test]
   fn test_zsumuj_dodatnie() {
      assert_eq!(zsumuj(3, 4), 7);
   }

   #[test]
   fn test_zsumuj_ujemne() {
      assert_eq!(zsumuj(-3, -4), -7);
   }
}
```
W powyższym przykładzie zostają przetestowane dwie różne sytuacje - dodawanie dwóch liczb dodatnich i dodawanie dwóch liczb ujemnych. Dzięki temu mamy pewność, że nasza funkcja działa zgodnie z oczekiwaniami dla różnych przypadków.

## Deep Dive

Istnieje wiele różnych możliwości w języku Rust, które możemy wykorzystać do pisania testów. Na przykład, możemy użyć makra `assert_ne!` aby sprawdzić, czy dwa wartości są różne, lub `assert!` aby sprawdzić pewne warunki. Możemy także grupować nasze testy za pomocą modułów, aby zachować porządek i łatwiejszą organizację.

Jednak pamiętajmy, że pisanie testów nie jest jedynie o sprawdzaniu poprawności naszego kodu, ale także o dokumentowaniu go. Używanie opisowych nazw funkcji testowych oraz komentarzy w kodzie może być bardzo pomocne dla nas i innych deweloperów, którzy będą czytać i pracować z naszym kodem w przyszłości.

## Zobacz też

Jeśli jesteś zainteresowany/dziwię się programowaniem w języku Rust, możesz przeczytać więcej o testowaniu i innych przydatnych narzędziach na stronie [Rust Book](https://doc.rust-lang.org/book/ch11-00-testing.html) oraz [official Rust documentation](https://doc.rust-lang.org/std/macro.assert.html).
Świetnym miejscem do poszukiwania informacji oraz zadawania pytań jest także [Rust Community Forum](https://users.rust-lang.org/). Powodzenia w pisaniu testów!