---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Interpolacja napisów to technika, umożliwiająca formatowanie i wstawianie zmiennych bezpośrednio do napisów w czasie wykonywania programu. Programiści stosują ją do uzyskania większej wydajności i czytelności kodu.

## Jak to zrobić:
Poniżej przedstawiam kilka przykładów dotyczących interpolacji łańcuchów w języku Rust.

```Rust
let name = "Jan"; 
let age = 25; 
println!("Cześć, mam na imię {} i mam {} lat.", name, age);
```

Wynik:
```
Cześć, mam na imię Jan i mam 25 lat.
```

## Szczegółowe informacje
Interpolacja łańcuchów nie jest nową techniką i jest powszechnie stosowana w wielu językach programowania. Rust jest jednak wyjątkowy w tym, że nie zezwala na bezpośrednią interpolację łańcuchów, jak w PHP czy JavaScript. Zamiast tego stosuje metody formatowania, które mają podobne możliwości, ale są bezpieczniejsze. 

Alternatywą dla interpolacji łańcuchów jest łączenie napisów za pomocą operatora `+` lub metody `.concat()`. Jednak ograniczeniem tych metod jest to, że obie działają tylko na napisach, natomiast interpolacja pozwala na wstawianie różnego rodzaju danych bezpośrednio do napisu.

## Ciekawe źródła
1. Dokumentacja języka Rust: https://doc.rust-lang.org/std/fmt/
2. Rust by Example: https://doc.rust-lang.org/rust-by-example/std/str.html
3. Artykuł o formatowaniu i wydrukach w Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html#format-and-print