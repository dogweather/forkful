---
title:    "Rust: Konwersja na wielkie litery w ciągu znaków."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zazwyczaj, gdy piszemy programy w Rust, chcemy mieć kontrolę nad naszymi danych. Czasami jednak potrzebujemy zmodyfikować pewne części danych. Na przykład, jeśli mamy ciąg znaków i chcemy zmienić jego pierwszą literę na wielką, możemy użyć funkcji do kapitalizacji. Bardzo przydatna jest więc umiejętność wykorzystania tej funkcji w naszych programach.

## Jak To Zrobić

Prosta metoda do obsługi capitalizacji w Rust to użycie funkcji `to_uppercase()` wraz z typem `String`. Oto prosty przykład kodu:

```Rust
let ciag_znakow = "witaj, jestem programem w języku Rust.";
let zmieniony_ciag = ciag_znakow
    .chars()
    .next()
    .map(|c| c.to_uppercase())
    .map(|upper_c| upper_c.collect::<String>())
    .map(|capitalized| capitalized + ciag_znakow[1..].to_string().as_str())
    .unwrap_or_default();
println!("Zmieniony ciąg: {}", zmieniony_ciag);
```

W tym przykładzie korzystamy z kilku metod, aby skorzystać z `to_uppercase()`. Najpierw zamieniamy ciąg znaków na iterator za pomocą metody `chars()`, a następnie wybieramy pierwszy znak i używamy `to_uppercase()` do zamiany go na wielką literę. Następnie przekształcamy go na typ `String`, aby móc połączyć go z resztą ciągu znaków. Na końcu drukujemy zmieniony ciąg. 

Mając to na uwadze, niektóre ciągi znaków mogą zawierać znaki specjalne lub niektóre języki mogą wymagać użycia metod specyficznych dla tego języka. W takim przypadku najlepiej skorzystać ze specjalistycznych bibliotek, takich jak `unicase`, aby zapewnić poprawność capitalizacji.

## Deep Dive

Funkcja `to_uppercase()` jest przede wszystkim zdefiniowana dla typu `char`. Możemy więc również użyć jej w naszych programach do capitalizacji pojedynczych znaków. Jest to możliwe dzięki temu, że są one reprezentowane w Unicode, a w nim każdy znak ma odpowiednią wielkość. Jednak istnieją znaki, których wielkość nie może być zmieniona, takie jak tzw. diakrytyczne znaki. Wtedy funkcja `to_uppercase()` zwróci ten sam znak, z którym została wywołana.

## Zobacz też

- Dokumentacja Rust: [https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase](https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase)
- Biblioteka `unicase`: [https://docs.rs/unicase/](https://docs.rs/unicase/)