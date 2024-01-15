---
title:                "Wydobywanie podciągów"
html_title:           "Rust: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w Rust, prawdopodobnie zdarzyło Ci się chcieć wyodrębnić kawałek tekstu ze stringa. Może to być fragment, który chcesz wyświetlić w innej części programu lub wyciągając dane z pliku. W tym artykule dowiecie się, jak wydobywać podciągi ze stringów w Rust i jak te operacje mogą pomóc w poprawie Twojego kodu.

## Jak to zrobić

W celu wyodrębnienia podciągu ze stringa w Rust, możemy użyć metody `get()` lub `slice()`. Metoda `get()` jest przydatna, jeśli chcemy wyodrębnić pojedynczą wartość, natomiast `slice()` pozwala nam na wyciągnięcie fragmentu tekstu o określonej długości.

```rust
let text = "To jest nasz przykładowy string";
let podciag = text.get(0..3); // "To "
let fragment = text.slice(11..18); // "przykład"
```

Jeśli chcesz wyodrębnić podciąg bez określania konkretnej długości, możemy użyć metody `find()` w celu znalezienia indeksu znaku, od którego chcemy zacząć wyodrębnianie.

```rust
let text = "Hello World!";
let index = text.find("W").unwrap();
let podciag = text.slice(index..); // "World!"
```

Możemy również użyć metody `split()` do podzielenia stringa na kilka podciągów na podstawie określonego separatora.

```rust
let text = "Jutro jest piękny dzień";
let splited = text.split(" "); // ["Jutro", "jest", "piękny", "dzień"]
```

## Deep Dive

Podczas wyodrębniania podciągów z stringów w Rust, warto zwrócić uwagę na wydajność i bezpieczeństwo naszego kodu. Jeśli znane nam jest dokładne miejsce, od którego chcemy zacząć wyodrębnianie, zawsze powinniśmy użyć metody `get()`, ponieważ jest ona bardziej wydajna.

Jeśli jednak nie jesteśmy pewni, w którym miejscu znajduje się nasz podciąg, lepiej użyć metody `slice()` lub `split()`, ponieważ nie wymagają one podawania konkretnych indeksów i są łatwiejsze w obsłudze.

## Zobacz również

- [Dokumentacja Rust: String](https://doc.rust-lang.org/std/string/index.html)
- [Poradnik: Wyodrębnianie podciągów w Rust](https://www.codementor.io/@rustacademy/string-slicing-in-rust-47hyiv5ns)