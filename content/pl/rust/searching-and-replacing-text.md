---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "Rust: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastąpienie tekstu jest częstym zadaniem podczas programowania. Może to być konieczne, gdy chcemy zmienić jakiś wyraz lub frazę w wielu miejscach w naszym kodzie lub plikach. Dzięki narzędziom takim jak Rust możemy wykonać to szybko i bezbłędnie, oszczędzając nam czas i wysiłek.

## Jak to zrobić

Zacznijmy od zaimportowania biblioteki 'regex' w naszym pliku Rust:
```Rust
use regex::Regex;
```
Następnie, przygotujmy nasz tekst, w którym chcemy dokonać zmian:
```Rust 
let text = "Witaj, świecie!";
```
Aby wykonać wyszukiwanie i zamianę tekstu, musimy określić wzorzec wyszukiwania oraz tekst, który będzie zastąpiony:
```Rust
let wzorzec = Regex::new("świecie").unwrap();
let nowy_tekst = wzorzec.replace(text, "Rusty świat").unwrap();
```
Warto zauważyć, że użyliśmy metody `unwrap()` na końcu każdego wywołania, aby obsłużyć potencjalne błędy. W tym przykładzie, zostanie wygenerowany nowy tekst: "Witaj, Rusty świat!".

Możemy również zastosować zmienne do naszego wzorca wyszukiwania, np. jeśli chcielibyśmy zmienić tylko pierwsze wystąpienie danego tekstu, które będzie zaczynało się dużą literą, możemy użyć następującego wzorca:
```Rust
let wzorzec = Regex::new("[^A-Z]+").unwrap(); //zastąp wszystkie znaki do pierwszej wielkiej litery
let nowy_tekst = wzorzec.replace(text, "Rusty").unwrap();
```
Oczywiście, istnieje wiele innych możliwości związanych z wyszukiwaniem i zastępowaniem tekstu w Rust. Zachęcamy do zapoznania się z dokumentacją biblioteki `regex`, aby poznać więcej opcji i sposobów jej wykorzystania.

## Głębsza analiza

W bibliotece `regex` dostępne są liczne funkcje, które pozwalają na precyzyjne i efektywne wyszukiwanie i zastępowanie tekstu. Na przykład, możemy użyć operatora "i" na końcu naszego wzorca, aby wyłączyć czułość na wielkość liter:
```Rust
let wzorzec = Regex::new("świecie").unwrap(); //jest czuły na wielkość liter
let nowy_tekst = wzorzec.replace(text, "Rusty świat").unwrap();
println!("{}", nowy_tekst); //Witaj, Rusty świat!

let wzorzec = Regex::new("(?i)świecie").unwrap(); //nieczuły na wielkość liter
let nowy_tekst = wzorzec.replace(text, "Rusty świat").unwrap();
println!("{}", nowy_tekst); //Witaj, Rusty świat!
```
Ponadto, biblioteka `regex` oferuje specjalne sekwencje, które można użyć w naszych wzorcach. Na przykład, sekwencja `\d` odpowiada dowolnej cyfrze, a sekwencja `\w` odpowiada dowolnej literze lub cyfrze. Możemy również użyć sekwencji `\s` do zastąpienia białych znaków, takich jak spacja lub tabulator. Przykładowo:
```Rust
let wzorzec = Regex::new("\\d+").unwrap();
let nowy_tekst = wzorzec.replace("Witaj, 123!", "Rusty").unwrap();
println!("{}", nowy_tekst); //Witaj, Rusty!
```

## Zobacz też

- [Dokumentacja biblioteki `regex`](https://docs.rs/regex/1.4.3/regex/)
- [Oficjalna strona