---
title:                "Rust: Usuwanie znaków pasujących do wzorca"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do wzorca jest ważnym aspektem przy pracy z tekstami w programowaniu. Może to być niezbędne, jeśli chcesz przetworzyć duże ilości tekstu lub wyeliminować błędy w danych. W tym artykule wyjaśnimy, jak można to osiągnąć przy użyciu języka programowania Rust.

## Jak to zrobić

W języku programowania Rust można użyć metody `replace` w kombinacji z warunkiem `matches` do usunięcia znaków pasujących do wzorca. Poniższy kod jest przykładem tego podejścia:

```Rust
let text = "Pomidor";
let new_text = text.replace(|c: char| c.is_uppercase(), ""); // wynik: "omidor"
```

W tym przykładzie wykorzystujemy metodę `replace`, która zastępuje znak `c` pustym ciągiem, jeśli spełnia on warunek `is_uppercase` (czyli czy jest wielką literą). W ten sposób usuwamy wszelkie wielkie litery z tekstu.

Inną metodą jest użycie biblioteki `regex`, która oferuje bardziej zaawansowane możliwości do usuwania znaków pasujących do wzorca. Poniżej znajduje się przykład kodu użycia biblioteki `regex`:

```Rust
use regex::Regex;

let text = "Kot1, Pies2, Kot3";
let re = Regex::new(r"\d").unwrap();
let new_text = re.replace_all(text, ""); // wynik: "Kot, Pies, Kot"
```

W tym przypadku użyliśmy wyrażenia regularnego `r"\d"`, które oznacza dowolną cyfrę. Następnie wykorzystaliśmy metodę `replace_all` do usunięcia wszystkich cyfr z tekstu.

## Deep Dive

Jeśli potrzebujesz bardziej zaawansowanej funkcjonalności, możesz również skorzystać z iteratorów w języku Rust. Przykład poniżej pokazuje, jak można użyć iteratora `filter` do usunięcia wszystkich znaków spełniających określony warunek:

```Rust
let text = "Zebra, Krowa, Koń";
let new_text: String = text.chars().filter(|c| c != &',' && c != &' ').collect(); // wynik: "ZebraKrowaKoń"
```

W tym przypadku wykorzystaliśmy metodę `chars` do zamiany tekstu na iterator pojedynczych znaków. Następnie użyliśmy metody `filter` z warunkiem, który usuwa wszystkie znaki różne od przecinka i spacji. Na koniec wywołaliśmy metodę `collect`, aby zamienić iterator z powrotem na ciąg znaków.

## Zobacz też

- Dokumentacja języka Rust: https://www.rust-lang.org/
- Biblioteka `regex`: https://docs.rs/regex/1.3.1/regex/