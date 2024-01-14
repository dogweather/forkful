---
title:                "Rust: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Dlaczego warto poszukiwać i zamieniać tekst w programowaniu Rust?

Poszukiwanie i zamienianie tekstu jest nieodłączną częścią procesu programowania w języku Rust. Pozwala nam to na szybkie i skuteczne zarządzanie tym, co wyświetla się w naszych programach, bez potrzeby ciągłego ręcznego edytowania plików. W tym artykule przedstawimy prosty sposób, w jaki możesz wykorzystać tę funkcję w swoim kodzie Rust.

## Jak to zrobić?

Aby zacząć poszukiwanie i zamienianie tekstu w Rust, musimy użyć metody `replace()` z biblioteki standardowej `std::string`. Przykład kodu znajduje się poniżej:

```Rust
let text = "Witaj, świecie!";
let new_text = text.replace("Witaj", "Cześć");
println!("{}", new_text);
```

Po uruchomieniu powyższego kodu, wyjściem będzie "Cześć, świecie!". Możemy również użyć wzorców w celu dokładniejszego określenia, co chcemy zamienić. Na przykład, możemy zmienić tylko pierwsze wystąpienie słowa "świecie" na "uniwersum":

```Rust
let new_text = text.replace("świecie", "uniwersum");
println!("{}", new_text);
```

W tym przypadku wyjściem będzie "Witaj, uniwersum!".

## Wszystko, co musisz wiedzieć o poszukiwaniu i zamienianiu tekstu

Funkcja `replace()` w języku Rust jest nie tylko przydatna w prostych przypadkach jak powyżej. Może być również wykorzystana do bardziej zaawansowanych operacji na tekście. Na przykład, możemy zastosować ją do dokładniejszej zmiany formatowania. Przykładowo, możemy zmienić wszystkie wielkie litery na małe, a jednocześnie pozostawić tylko pierwszą literę z dużą:

```Rust
let text = "PROGRAMOWANIE uczy pokory";
let new_text = text.replace("PROGRAMOWANIE", "programowania");
println!("{}", new_text);
```

W wyniku otrzymamy "Programowania uczy pokory". 

## Odkryj więcej możliwości

Funkcja `replace()` to tylko jedna z wielu dostępnych opcji do wyszukiwania i zamieniania tekstu w języku Rust. Mocne wsparcie dla operacji na tekście jest kluczowe w wielu aspektach programowania, dlatego zachęcamy do dalszego eksplorowania i wykorzystywania tej funkcji w swoich projektach. 

# Zobacz również

- Dokumentacja Rust: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Stack Overflow: https://stackoverflow.com/questions/28392008/how-can-i-replace-a-substring-inside-a-string
- Ruszaj w świat Rust: https://www.rust-lang.org/learn