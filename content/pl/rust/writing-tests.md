---
title:                "Pisanie testów"
html_title:           "Rust: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne?

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki nim możemy mieć pewność, że nasz kod działa poprawnie i zgodnie z naszymi oczekiwaniami. Testowanie jest również kluczowym elementem w walce z błędami, ponieważ pozwala nam szybko wykryć i poprawić ewentualne problemy.

## Jak pisać testy w języku Rust?

Pisanie testów w języku Rust jest bardzo proste i intuicyjne. Wystarczy skorzystać z wbudowanej biblioteki `test` i jej makr. Przykładowy kod wyglądałby tak:

```Rust
#[test]
fn test_addition() {
    let result = add_numbers(2, 3);
    assert_eq!(result, 5);
}
```

W powyższym przykładzie widzimy funkcję testową `test_addition`, która używa makra `assert_eq!` do porównania oczekiwanego wyniku z faktycznym wynikiem funkcji `add_numbers`. Aby uruchomić ten test, wystarczy użyć polecenia `cargo test` w terminalu.

### Przydatne makra

W języku Rust istnieje wiele przydatnych makr, które ułatwiają pisanie testów. Niektóre z nich to:

- `assert!` - pozwala na sprawdzenie warunku logicznego
- `assert_eq!` - porównuje dwa wyrażenia
- `assert_ne!` - porównuje dwa wyrażenia i upewnia się, że są one różne
- `should_panic` - sprawdza, czy funkcja zgłasza oczekiwany błąd

Więcej informacji na temat dostępnych makr i ich zastosowania można znaleźć w oficjalnej dokumentacji języka Rust.

## Głębsze spojrzenie na pisanie testów

Testowanie w języku Rust to znacznie więcej niż tylko używanie wbudowanych makr. Istnieją pewne dobre praktyki, które warto przestrzegać podczas pisania testów. Kilka z nich to:

- Separacja kodu - testy powinny być pisane oddzielnie od samego kodu źródłowego oraz powinny być zgrupowane w osobnym folderze.
- Testowanie wszystkich możliwych przypadków - należy pamiętać o testowaniu różnych przypadków, w szczególności tych krańcowych.
- Nazewnictwo funkcji testowych - nazwy funkcji testowych powinny zaczynać się od słowa `test` oraz dobrze opisywać przetestowaną funkcjonalność.

Pamiętajmy, że testy to również część naszego kodu i powinny być pisane zgodnie z zasadami czystego kodu.

## Zobacz także

- [Dokumentacja języka Rust](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Przewodnik po testowaniu w języku Rust](https://medium.com/@ericdreichert/starting-with-rust-004-testing-b5a30e9a9072)
- [Wideo na temat testowania w języku Rust](https://www.youtube.com/watch?v=sAxiUw3Vkbs)