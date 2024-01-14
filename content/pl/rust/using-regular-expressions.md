---
title:                "Rust: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego
Regularne wyrażenia, w skrócie zwane "regex", są powszechnym i potężnym narzędziem w programowaniu. Pozwalają one na szybkie i precyzyjne przeszukiwanie oraz manipulowanie tekstem. W tym artykule dowiecie się, dlaczego warto używać regexów w swoich programach.

## Jak to zrobić
Aby użyć regularnych wyrażeń w języku Rust, należy zaimportować bibliotekę ```regex```. Poniżej przedstawione są przykłady kodu, które demonstrują podstawowe funkcjonalności regexów:

```
use regex::Regex;

// Tworzenie nowego wyrażenia regularnego
let re = Regex::new(r"ab+c").unwrap();

// Sprawdzanie czy dany tekst pasuje do wzorca
assert_eq!(re.is_match("ac"), false);
assert_eq!(re.is_match("abbbc"), true);

// Wyszukiwanie dopasowań w tekście
let text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
let matches = re.find_iter(text);
for mat in matches {
    println!("Dopasowanie znalezione na pozycji: {}", mat.start());
}
```

Powyższy kod znajduje dopasowania stringa "ab+b" w tekście i wyświetla ich pozycje. Więcej informacji na temat różnych metod i opcji regularnych wyrażeń w Rust można znaleźć w oficjalnej dokumentacji libray ```regex```.

## Głębszy zanurzony

Regularne wyrażenia są wyjątkowo użyteczne przy przetwarzaniu tekstu, szczególnie gdy próbujemy wyodrębnić konkretne informacje lub zmienić strukturę tekstu. Z ich pomocą możemy wykonać wiele zadań, takich jak:

- Sprawdzenie czy adres email jest poprawnie sformatowany
- Wyodrębnienie numeru telefonu z danego tekstu
- Zastąpienie niepoprawnych znaków w tekście
- Analiza składniowa złożonych plików tekstowych

Warto pamiętać, że regexy mogą być trudne do zrozumienia i wymagają praktyki, aby je opanować. Dlatego, warto poświęcić czas na naukę ich obsługi, ponieważ mogą one znacznie ułatwić pracę podczas tworzenia programów.

## Zobacz również
- [Dokumentacja biblioteki regex w języku Rust](https://docs.rs/regex/1.4.2/regex/)
- [Oficjalna strona języka Rust](https://www.rust-lang.org/en-US/)
- [Poradnik dla początkujących w języku Rust](https://doc.rust-lang.org/book/)