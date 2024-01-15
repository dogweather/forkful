---
title:                "Usuwanie znaków odpowiadających wzorcowi"
html_title:           "Rust: Usuwanie znaków odpowiadających wzorcowi"
simple_title:         "Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do określonego wzorca jest częstym zadaniem podczas pisania programów. Jest to bardzo przydatna umiejętność w programowaniu, ponieważ pozwala na szybkie i skuteczne oczyszczanie danych oraz przetwarzanie ich zgodnie z wymaganiami.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca w języku Rust, możemy skorzystać z metody `chars()` i funkcji `filter()` na łańcuchu znaków. Wygląda to następująco:

```Rust
let string = "Przykład tekstu 123";
let filtered_string: String = string.chars().filter(|c| c.is_alphabetic()).collect();
println!("String po filtrowaniu: {}", filtered_string);
```

W powyższym przykładzie najpierw tworzymy zmienną `string` zawierającą przykładowy ciąg znaków, a następnie tworzymy nową zmienną `filtered_string`, która będzie przechowywać wynik filtrowania. Wykorzystujemy metodę `chars()` do pobrania każdego pojedynczego znaku z łańcucha `string`, a następnie używamy funkcji `filter()`, która pozwala nam określić warunek, który musi zostać spełniony, aby dany znak został zachowany. W tym przypadku warunek to `is_alphabetic()`, co oznacza, że tylko znaki alfabetyczne będą zachowane. Na koniec wykorzystujemy funkcję `collect()` do zapisania wyników filtrowania do zmiennej `filtered_string` i wyświetlamy ją za pomocą funkcji `println!()`.

## Przyjrzenie się bliżej

W powyższym przykładzie użyliśmy metody `chars()` i funkcji `filter()` w celu usunięcia znaków pasujących do naszego wzorca. Metoda `chars()` pozwala na iterację po łańcuchu znaków, a funkcja `filter()` służy do filtrowania tych znaków. Możemy również użyć innych metod, takich jak `map()`, `fold()` czy `reduce()`, aby przetworzyć nasze dane na inne sposoby. Istnieje wiele różnych możliwości, więc zachęcamy do eksperymentowania i poszukiwania najlepszych rozwiązań dla swoich potrzeb.

## Zobacz też

- [Dokumentacja języka Rust](https://www.rust-lang.org/)
- [Kurs programowania w języku Rust](https://www.rust-lang.org/learn)
- [Przykładowe projekty w języku Rust](https://github.com/rust-lang-ru/awesome-rust-projects)