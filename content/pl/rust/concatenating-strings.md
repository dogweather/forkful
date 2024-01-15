---
title:                "Łączenie ciągów znaków"
html_title:           "Rust: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy łączyć ciągi znaków w Rust? Po prostu dlatego, że często musimy łączyć różne fragmenty tekstu w jedną całość, na przykład w celu wyświetlenia danych użytkownika czy generowania raportów. W tym artykule dowiesz się, w jaki sposób można to łatwo i wydajnie osiągnąć w języku Rust.

## Jak

```Rust
// Definiowanie ciągów znaków
let imie = "Anna";
let nazwisko = "Kowalska";

// Proste łączenie za pomocą operatora "+"
let pelne_imie = imie + " " + nazwisko;
println!("{}", pelne_imie); // Output: Anna Kowalska

// Zastosowanie makra "format!"
let pelne_imie = format!("{} {}", imie, nazwisko);
println!("{}", pelne_imie); // Output: Anna Kowalska

// Użycie metody "to_string()"
let pelne_imie = imie.to_string() + " " + &nazwisko.to_string();
println!("{}", pelne_imie); // Output: Anna Kowalska
```

Mając dwa ciągi znaków, można je wygodnie połączyć za pomocą operatora "+" lub makra "format!". W przypadku gdy jeden z ciągów jest typu String, należy przekonwertować drugi ciąg na ten typ przy pomocy metody "to_string()".

## Deep Dive

W języku Rust istnieje także struktura "String" pozwalająca na wygodniejsze zarządzanie i modyfikowanie ciągami znaków. Można ją utworzyć na podstawie już istniejącego ciągu za pomocą metody "to_string()":

```Rust
let imie = "Anna";
let imie_string = imie.to_string(); // Tworzenie String z ciągu znaków

// Dodawanie tekstu na końcu za pomocą metody "push_str()"
let mut pelne_imie = imie_string;
pelne_imie.push_str(" Kowalska"); // " " i część przedrostkowa są wymagane
println!("{}", pelne_imie); // Output: Anna Kowalska

// Wstawianie tekstu w dowolne miejsce za pomocą metody "insert()"
let imie_string = imie.to_string(); // Tworzenie String z ciągu znaków
let mut pelne_imie = format!("{} Ochmańska", imie_string);

// length() zwraca ilosc znaków, numeracją pamiętajmy od zera
pelne_imie.insert(imie_string.len(), 'n'); // wstawienie "n" za " Anna Ochmańska"
println!("{}", pelne_imie); // Output: Anna N Ochmańska
```

Struktura "String" oferuje wiele przydatnych metod, takich jak "push_str()", "insert()", "replace()", czy "remove()", które ułatwiają manipulowanie ciągami znaków.

### See Also

Jeśli chcesz dowiedzieć się więcej o manipulowaniu ciągami znaków w Rust, warto zapoznać się z dokumentacją języka lub skorzystać z poniższych linków:

- [Dokumentacja języka Rust](https://doc.rust-lang.org/std/string/index.html)
- [Wideo-tutorial "Strings in Rust" autorstwa Derek Banas](https://www.youtube.com/watch?v=zxjCrhxFQ-c)
- [Blogpost dotyczący łączenia String i str w Rust](https://programming-idioms.org/idiom/14/concatenate-strings/4185/rust)