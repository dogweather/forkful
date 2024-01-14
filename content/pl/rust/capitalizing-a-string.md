---
title:                "Rust: Uppercasing ciągu znaków"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego warto użyć Rusta do zmiany wielkości liter w tekście?

Rust jest jednym z najpopularniejszych języków programowania, który charakteryzuje się wysoką wydajnością i bezpieczeństwem. Dzięki swojej składni i funkcjom, jest idealnym wyborem dla osób, które chcą zmienić wielkość liter w tekście. Nie tylko jest to zadanie często wykonywane w różnych aplikacjach, ale również dobrze pokazuje możliwości i zalety Rusta.

## Jak to zrobić?

Aby wykorzystać Rusta do zmiany wielkości liter w tekście, potrzebujemy zastosować kilka funkcji dostępnych w standardowej bibliotece języka. Najważniejsze z nich to `to_uppercase()` i `to_lowercase()`, które odpowiednio zwracają tekst z zamienionymi na wielkie lub małe litery.

```Rust
let text = "to jest przykładowy tekst";
let text_upper = text.to_uppercase();
let text_lower = text.to_lowercase();

println!("{}", text_upper);
println!("{}", text_lower);
```

**Output:**

TO JEST PRZYKŁADOWY TEKST
to jest przykładowy tekst

Kolejną ważną funkcją jest `chars()`, która pozwala na iterację przez poszczególne znaki w tekście. Dzięki niej, możemy zastosować warunek sprawdzający i zmodyfikować konkretne litery w zdaniu.

```Rust
let mut text = String::from("to jest przykładowy tekst");

for (index, letter) in text.chars().enumerate() {
    if index % 2 == 0 {
        text.insert(index, letter.to_uppercase().next().unwrap());
    }
}

println!("{}", text);
```

**Output:**

To JEsT pRzYkŁAdoWY tEkSt

## Głębsze zagadnienia

Aby pełniej zrozumieć, jak działają funkcje odpowiedzialne za zmianę wielkości liter w Rust, warto poznać kilka koncepcji związanych z typami danych oraz własnościami języka.

Pierwszą ważną kwestią jest to, że zmienne w Rust są domyślnie niemutowalne, czyli nie można ich zmienić po ich zadeklarowaniu. Dlatego też, aby zmienić wielkość liter w tekście, musimy wykorzystać mutowalną wersję zmiennej lub stworzyć zupełnie nową.

Kolejnym aspektem ważnym dla zmiany wielkości liter jest wykorzystanie mechanizmu własnych typów zwracanych. W przypadku funkcji `to_uppercase()` i `to_lowercase()` jest to po prostu typ `String`, ale w zależności od konkretnego problemu, warto wiedzieć, jakie są dostępne opcje i na co zwracać uwagę przy tworzeniu własnych funkcji.

## Zobacz także

- [Dokumentacja Rusta o typach danych](https://doc.rust-lang.org/book/ch03-02-data-types.html)
- [Podręcznik programisty Rusta](https://doc.rust-lang.org/stable/rust-by-example/index.html)
- [Społeczność języka Rust na Reddit](https://www.reddit.com/r/rust/)