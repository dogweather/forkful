---
title:    "Rust: Usuwanie znaków pasujących do wzorca"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania kodu w Rust, może zdarzyć się, że potrzebujemy usunąć pewne znaki, które pasują do określonego wzorca. To może być spowodowane różnymi przyczynami, na przykład potrzebą oczyszczenia danych, zmniejszenia rozmiaru plików lub po prostu poprawieniem estetyki kodu. W tym artykule dowiesz się, jak można wykonać to zadanie w języku Rust.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca w Rust, musimy użyć metod dostępnych w standardowej bibliotece języka. Przykładowe kody poniżej przedstawiają dwa sposoby realizacji tego zadania.

```Rust
let string = String::from("Programming in Rust is fun!");
let result = string.replace("in Rust", "");
println!("{}", result); // Output: "Programming is fun!"
```

W powyższym przykładzie, użyliśmy metody `replace()`, która zwraca nowy `String` bez wybranego fragmentu tekstu. Możemy również wykorzystać metodę `drain()`, która pozostawia oryginalny `String` i usuwa tylko wybraną część.

```Rust
let mut string = String::from("Programming in Rust is fun!");
let range = 14..20;
string.drain(range);
println!("{}", string); // Output: "Programming is fun!"
```

## Deep Dive

W języku Rust, znaki są reprezentowane przez typ `char`, a nie `char` dla pojedynczych bajtów, dlatego nie możemy po prostu przefiltrować obiektu `String` i usunąć znaków na podstawie ich indeksów. Musimy użyć specjalnych metod, takich jak `replace()` lub `drain()`, aby wykonac to zadanie. Ponadto, jednakże, powinniśmy pamiętać, że wielkość liter jest traktowana jako różna podczas usuwania znaków pasujących do wzorca.

## Zobacz również

- Dokumentacja Rust na temat typu `String`: https://doc.rust-lang.org/std/string/struct.String.html
- Przykładowe kody z tego artykułu: https://github.com/example-repository
- Inne artykuły o programowaniu w języku Rust: https://example-blog.pl/tag/rust