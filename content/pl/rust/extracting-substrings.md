---
title:                "Rust: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ekstrakcja podciągów jest często wykorzystywaną funkcją w programowaniu i może mieć wiele zastosowań. Może być wykorzystywana do filtrowania danych, manipulowania tekstami lub tworzenia wygodnych funkcji dla użytkowników. W tym artykule dowiesz się, dlaczego warto nauczyć się tej funkcji w języku Rust.

## Jak to zrobić

Aby wydobyć podciągi w języku Rust, można użyć metody `slice()` lub notacji indeksowej `[]`. Spójrzmy na przykładowy kod:

```Rust
let text = "To jest przykładowy tekst";
let extracted = &text[4..11];

println!("{}", extracted);
```

Rezultatem powyższego kodu będzie wydrukowanie podciągu "jest pr". Możemy także użyć metody `find()` w celu znalezienia konkretnego znaku lub podciągu w tekście. Poniżej przedstawiono przykład:

```Rust
let text = "To jest przykładowy tekst";
let found = text.find("przykładowy");

println!("{}", found);
```

Wynikiem powyższego kodu będzie wydrukowanie indeksu, na którym znajduje się szukany podciąg, w tym przypadku będzie to wartość 8.

## Głębszy zanurzenie

W języku Rust, podciągi są reprezentowane przez typ danych `&str`, który jest odpowiednikiem dla typu `String` zmutowalnego. Jednak wydobyte podciągi nie będą zmutowane w oryginalnym tekście, co jest ważne do zapamiętania.

Istnieje kilka rzeczy, które warto zauważyć przy korzystaniu z funkcji ekstrakcji podciągów w języku Rust:

1. Indeksowanie rozpoczyna się od zera, dlatego przedział `[4..11]` z poprzedniego przykładu właściwie odnosi się do znaków od 5 do 10 w tekście, ponieważ znak o indeksie 11 nie jest uwzględniony.
2. Wydobywanie podciągów poza zakresem spowoduje błąd wykonania programu, dlatego warto upewnić się, że wybrany zakres jest poprawny.
3. Metoda `find()` zwróci `None` jeśli nie znajdzie szukanego znaku lub podciągu, dlatego należy sprawdzić czy wartość jest dostępna przed wydrukowaniem lub wykorzystaniem w dalszej części kodu.

## Zobacz też

- Dokumentacja Rust na temat funkcji ekstrakcji podciągów: https://doc.rust-lang.org/std/primitive.str.html#method.slice
- Przydatne wskazówki na temat pracy z podciągami w Rust: https://www.newline.co/write-a-safe-string-slice-in-rust/