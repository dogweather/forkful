---
title:    "Rust: Pisanie testów"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów to nie tylko ważny element procesu programowania, ale także sposób na zapewnienie jakości i niezawodności naszego kodu. Testy pozwalają upewnić się, że nasz program działa zgodnie z naszymi oczekiwaniami i pomagają szybko zlokalizować ewentualne błędy. W tym artykule dowiesz się, dlaczego warto pisać testy w języku Rust.

## Jak to zrobić

Pisanie testów w języku Rust jest bardzo proste i intuicyjne. Możesz zacząć od utworzenia nowego projektu lub edycji już istniejącego. Następnie stwórz nowy plik z rozszerzeniem `.rs`, w którym będziemy umieszczać nasze testy.

Poniżej przedstawiamy przykładowy kod testu, który sprawdza, czy funkcja `calculate_sum()` poprawnie sumuje liczby:

```Rust
fn calculate_sum(numbers: &[i32]) -> i32 {
    let mut sum = 0;

    for num in numbers {
        sum += num;
    }

    return sum;
}

#[test]
fn test_calculate_sum() {
    let numbers = [1, 3, 5, 7, 9];
    assert_eq!(calculate_sum(&numbers), 25);
}

```

Możemy zauważyć, że przed nazwą funkcji testowej mamy atrybut `#[test]`, który informuje kompilator, że dany kod jest testem. Wewnątrz funkcji testowej używamy makra `assert_eq!()`, aby porównać oczekiwany wynik z rzeczywistym. Jeśli wartości są różne, test nie przejdzie, co pomoże nam znaleźć błąd w kodzie.

## Deep Dive

Możliwość pisanie testów w języku Rust jest bardzo rozbudowana, co daje nam wiele możliwości. Możemy na przykład tworzyć testy jednostkowe, integracyjne czy też testy wydajnościowe. Rust posiada również specjalne makra, które ułatwiają pisanie testów, na przykład `assert_eq_ne!()` pozwalające sprawdzić, czy wartości są różne.

Pamiętaj, że testy powinny pokrywać jak najwięcej części naszego kodu, aby zapewnić kompleksowe sprawdzenie jego działania. Dzięki temu będziemy mieć większą pewność, że nasz program działa poprawnie.

## Zobacz także

- Dokumentacja projektu Rust: https://www.rust-lang.org/pl/learn 
- Przykładowe projekty z testami w języku Rust: https://github.com/rust-lang/rustlings