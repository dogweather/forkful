---
title:                "Analizowanie daty z ciągu znaków"
html_title:           "Rust: Analizowanie daty z ciągu znaków"
simple_title:         "Analizowanie daty z ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Cześć programiści! Czy kiedykolwiek zdarzyło Ci się otrzymać datę jako ciąg znaków i miało problem z jej sparsowaniem? Nie martw się, nie jesteś sam! W dzisiejszym artykule dowiesz się, czym jest parsowanie daty z ciągu znaków i dlaczego jest to ważna umiejętność dla programistów. Będziemy również omawiać, jak to zrobić w języku Rust.

## Co i dlaczego?
Parsowanie daty z ciągu znaków to proces konwertowania daty wyrażonej jako tekst na inny format lub typ danych. Jest to ważne, ponieważ wiele aplikacji musi obsługiwać daty w różnych formatach i trzeba umieć je przetwarzać, aby poprawnie wyświetlać lub porównywać. Bez tego umiejętności, manipulowanie danymi datowymi może być bardzo trudne i prowadzić do błędów.

## Jak to zrobić:
Aby sparsować datę z ciągu znaków w Rust, możemy wykorzystać pakiet "chrono". Wystarczy dodać go do pliku Cargo.toml i zaimportować do kodu:

```Rust
[dependencies]
chrono = { version = "0.4", features = ["serde", "rustc-serialize"] }
```

W ten sposób możemy użyć biblioteki w naszym kodzie:

```Rust
use chrono::prelude::*;

let date_str = "20-04-2021";
let date = NaiveDate::parse_from_str(date_str, "%d-%m-%Y").unwrap();

println!("{}", date.format("%A, %B %e, %Y"));
```

Ten kod sparsuje datę z ciągu "20-04-2021" i wyświetli ją w formacie "Wtorek, Kwiecień 20, 2021". Jest to tylko przykład, możesz eksperymentować z różnymi formatami i funkcjami, aby uzyskać pożądane wyniki.

## Pogłębione informacje:
Parsowanie daty z ciągu znaków jest często ważnym elementem w aplikacjach, które wymagają manipulacji datami. W przeszłości, proces ten był bardziej skomplikowany i wymagał użycia specjalnych bibliotek lub ręcznego przetwarzania danych. W języku Rust, dzięki pakietowi "chrono", tworzenie i przetwarzanie dat jest znacznie prostsze.

Alternatywnym sposobem jest wykorzystanie pakietu "regex" do wielokrotnego porównywania ciągów znaków z różnymi formatami daty. Jednak używanie biblioteki "chrono" jest zazwyczaj łatwiejsze i bardziej intuicyjne.

Jeśli jesteś ciekawy, jak dokładnie działa biblioteka "chrono" i jakie inne funkcje oferuje, warto przejrzeć jej dokumentację dostępną na stronie internetowej Rust.

## Zobacz również:
- [Dokumentacja pakietu "chrono"](https://docs.rs/chrono/0.4.19/chrono/)
- [Przykładowy kod parsowania daty w języku Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=180b7ba5220346bc0a3d8420422c0af2)