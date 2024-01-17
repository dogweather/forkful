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

## Co to jest i dlaczego? 
Testowanie jest procesem, którego celem jest sprawdzenie, czy kod działa zgodnie z założeniami. Programiści często piszą testy, aby upewnić się, że ich kod jest poprawny i nie zawiera błędów. Testowanie jest ważnym elementem w procesie tworzenia oprogramowania, ponieważ pozwala na wcześniejsze wykrycie błędów i ułatwia utrzymanie jakości kodu.

## Jak to zrobić: 
W języku Rust do tworzenia testów można wykorzystać makra ```assert!``` i ```assert_eq!```, które pozwalają na porównywanie wartości i warunków. Przykładowa implementacja testu wyglądałaby następująco:

```Rust
fn divisible_by_five(num: u32) -> bool { 
    num % 5 == 0 
} 

// test
#[test]
fn test_divisible_by_five() {
    assert!(divisible_by_five(10)); // prawidłowy wynik
    assert_eq!(divisible_by_five(12), false); // błędny wynik
}
```

Wywołanie testu odbywa się poprzez uruchomienie komendy ```cargo test```, która wyświetla informacje o wynikach testów.

## Pełniejsze spojrzenie: 
Testowanie jest praktykowane od lat i jest nieodłącznym elementem procesu rozwoju oprogramowania. Alternatywą dla pisania testów jest testowanie manualne, lecz jest to bardziej czasochłonny i mniej precyzyjny proces. W języku Rust testy są często wykorzystywane w połączeniu z systemem CI/CD, który automatycznie uruchamia je po każdej zmianie w kodzie.

## Zobacz również:
- Dokumentacja Rust na temat testowania: https://doc.rust-lang.org/book/ch11-00-testing.html
- Przykładowy projekt z wykorzystaniem testów w Rust: https://github.com/hyperium/hyper
- Porównanie pomiędzy testowaniem manualnym a automatycznym w oprogramowaniu: https://www.business2community.com/tech-gadgets/manual-testing-vs-automated-testing-exactly-different-01438198