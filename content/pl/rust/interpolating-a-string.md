---
title:                "Interpolowanie ciągu znaków"
html_title:           "Rust: Interpolowanie ciągu znaków"
simple_title:         "Interpolowanie ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest interpolowanie ciągów znaków i dlaczego programiści to robią?
Interpolowanie ciągów znaków w języku Rust to sposób na łączenie zmiennych z ciągiem znaków. Jest to przydatne w sytuacji, gdy chcemy wyświetlić zmienną wewnątrz tekstu. Programiści korzystają z interpolacji ciągów znaków, aby uniknąć tworzenia wielu ciągów znaków, co może być uciążliwe i nieczytelne.

## Jak to zrobić?
```Rust
let name = "Jan";
println!("Cześć, {}", name);
```
Output:
```
Cześć, Jan
```
W powyższym przykładzie zmienna ```name``` została wstawiona wewnątrz ciągu znaków za pomocą nawiasów ```{}```, a następnie wyświetlona przy użyciu funkcji ```println!()```. Możemy odwoływać się do dowolnej liczby zmiennych w ten sam sposób, zawsze zaczynając od znaku ```{}```.

## Głębsze spojrzenie
Interpolowanie ciągów znaków jest popularną techniką nie tylko w języku Rust, ale także w innych językach programowania, takich jak Python czy JavaScript. W języku Rust zmienna używana w interpolowanym ciągu musi implementować operator ```Display```, który określa, jak zmienna ma być reprezentowana jako ciąg znaków.

Alternatywą dla interpolowania ciągów znaków jest funkcja ```format!()```, która także pozwala na łączenie zmiennych z ciągiem znaków, ale w nieco inny sposób. Interpolowanie jest zazwyczaj wybierane ze względu na swoją krótszą składnię i prostsze użycie.

## Zobacz także
- [Dokumentacja interpolacji ciągów znaków w języku Rust](https://doc.rust-lang.org/std/fmt/index.html#formatting-parameters)
- [Porównanie interpolacji i funkcji format! w języku Rust](https://brson.github.io/2016/11/30/interpolation-vs-format)