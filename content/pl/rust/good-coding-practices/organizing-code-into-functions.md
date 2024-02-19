---
aliases:
- /pl/rust/organizing-code-into-functions/
date: 2024-01-26 01:16:15.506147-07:00
description: "Organizowanie kodu w funkcje polega na rozbijaniu programu na wielokrotnie\
  \ u\u017Cywane, modu\u0142owe kawa\u0142ki identyfikowane przez nazw\u0119. Robimy\
  \ to, aby nasz kod\u2026"
lastmod: 2024-02-18 23:08:49.395404
model: gpt-4-0125-preview
summary: "Organizowanie kodu w funkcje polega na rozbijaniu programu na wielokrotnie\
  \ u\u017Cywane, modu\u0142owe kawa\u0142ki identyfikowane przez nazw\u0119. Robimy\
  \ to, aby nasz kod\u2026"
title: Organizowanie kodu w funkcje
---

{{< edit_this_page >}}

## Co i dlaczego?
Organizowanie kodu w funkcje polega na rozbijaniu programu na wielokrotnie używane, modułowe kawałki identyfikowane przez nazwę. Robimy to, aby nasz kod był czyściejszy, bardziej czytelny i łatwiejszy do debugowania. Chodzi o to, aby nie powtarzać się i ułatwiać aktualizacje.

## Jak to zrobić:
Wyobraź sobie, że masz kod, który kilka razy oblicza powierzchnię koła. Zamiast powtarzać formułę, zamykasz ją w funkcji.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("Powierzchnia koła wynosi: {}", area);
}
```

Wynik:

```
Powierzchnia koła wynosi: 78.53981633974483
```

## Dogłębna analiza
Historycznie, funkcje pochodzą z matematyki, gdzie przyporządkowują wejścia do wyjść. W kodowaniu są obecne już od czasów asemblera, chociaż nazywaliśmy je „podprogramami”. Funkcje w Rust mogą zwracać wartości, a nawet inne funkcje dzięki funkcjom pierwszoklasowym i domknięciom.

Alternatywy? Kod wbudowany lub makra, ale są bałaganem dla skomplikowanej logiki. Obiekty z metodami to inny sposób organizowania funkcjonalności, inny niż samodzielne funkcje.

Implementacja w Rust jest dość prosta. Funkcje deklarują typy swoich parametrów i typ zwracany. Przyjęło się, że nazwy są pisane 'snake case'. Masz publiczne funkcje (`pub fn`) do użytku poza modułem i prywatne do użytku wewnętrznego. I Rust ma tę fajną funkcję, że nie potrzebujesz słowa kluczowego `return` dla ostatniego wyrażenia w funkcji.

## Zobacz też
Sprawdź te materiały po więcej informacji:
- Książka o języku programowania Rust: [Funkcje](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust przykładami na temat [Funkcji](https://doc.rust-lang.org/rust-by-example/fn.html)
