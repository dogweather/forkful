---
title:                "Analiza składni HTML"
html_title:           "Rust: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsowanie HTML-u jest częstym wyzwaniem, z którym spotykają się programiści przy tworzeniu aplikacji internetowych. Wprowadzając Rust, język z wydajną składnią i narzędziami do obsługi błędów, możemy uprościć ten proces i uzyskać niezawodne wyniki.

## Jak to zrobić

Pierwszym krokiem w parsowaniu HTML-u jest pobranie zawartości strony internetowej. Możemy to zrobić za pomocą biblioteki `reqwest`, importując ją do naszego projektu:

```Rust
use reqwest::blocking::get;
```

Następnie, używając metody `get()` i podając adres URL, pobieramy zawartość strony i zapisujemy ją do zmiennej:

```Rust
let res = get("https://example.com").unwrap();
let body = res.text().unwrap();
```

Teraz, aby przetworzyć ten dokument HTML w sposób łatwy i niezawodny, możemy skorzystać z biblioteki `select`, którą również importujemy do naszego projektu:

```Rust
use select::document::Document;
use select::predicate::Name;
```

Korzystając z metody `Document::from()` możemy przekazać pobraną zawartość strony i uzyskać obiekt typu `Document`. Następnie, wewnątrz bloku `for` i korzystając z predykatu `Name()`, możemy iterować po wszystkich elementach HTML i wybierać tylko te, które nas interesują:

```Rust
for p in Document::from(&body).find(Name("p")) {
    println!("{}", p.text());
}
```

Przykładowy output może wyglądać tak:

```
Pierwszy paragraf.
Drugi paragraf.
Trzeci paragraf.
```

## Głębsze zagadnienia

Parsowanie HTML-a może być skomplikowanym i czasochłonnym procesem, szczególnie w przypadku stron o różnych strukturach i zagnieżdżonych elementów. Dlatego warto zapoznać się z dokumentacją biblioteki `select` oraz innymi dostępnymi narzędziami, takimi jak `scraper`, `html5ever` czy `htmlbird`.

## Zobacz także

- [Dokumentacja biblioteki `select`](https://docs.rs/select)
- [Porównanie parserów HTML w Rust](https://perf.rust-lang.org/html-parsing-comparison/)
- [Kod źródłowy przykładowego projektu do parsowania HTML w Rust](https://github.com/example/rust-html-parser)