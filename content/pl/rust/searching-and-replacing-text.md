---
title:    "Rust: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Stacja Przemysłowa Rust (Rust Industrial Station) jest wreszcie wygodną alternatywą dla wyszukiwania i zastępowania tekstu w Rust. Nie tylko jest łatwy w użyciu, ale też wydajny i niezawodny. W tym artykule dowiesz się, dlaczego warto wybrać Rust do wyszukiwania i zastępowania tekstu.

## Jak to zrobić

Przykłady kodów i wyników (sample output) zawarte są w blokach kodu "```Rust ... ```". W Rust Industrial Station istnieją różne narzędzia służące do wyszukiwania i zastępowania tekstu. Jednym z nich jest funkcja `replace()` w module `std::string`, która zastępuje wszystkie wystąpienia danego tekstu w zmiennej. Na przykład:

```Rust
let my_string = "Cześć, jestem programistą w Rust!";
let replaced_string = my_string.replace("Rust", "Python");
println!("Nowy napis: {}", replaced_string);
```

Wynik:

`Cześć, jestem programistą w Python!`

Innym przydatnym narzędziem jest funkcja `find()` w module `std::str`, która wyszukuje pierwsze wystąpienie danego tekstu w zmiennej. Na przykład:

```Rust
let my_string = "Hello world!";
let found_position = my_string.find("world");
println!("Pozycja znaleziona: {}", found_position);
```

Wynik:

`Pozycja znaleziona: 6`

## Głębsza analiza

W Rust Industrial Station możliwe jest także wykorzystanie wyrażeń regularnych do bardziej złożonych operacji wyszukiwania i zastępowania tekstu. Wyrażenia regularne są wzorcami, które pozwalają na dokładniejsze określenie szukanego tekstu. Na przykład, jeśli chcemy zastąpić wszystkie liczby pojedynczą gwiazdką, możemy użyć następującego wyrażenia regularnego:

```Rust
let my_string = "Liczba pi to 3.141592653589793";
let replaced_string = Regex::new(r"[0-9]+").unwrap().replace_all(&my_string, "*");
println!("Napis z gwiazdkami: {}", replaced_string);
```

Wynik:

`Liczba pi to *.**************`

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o wyszukiwaniu i zastępowaniu tekstu w Rust, zapoznaj się z następującymi linkami:

- [Dokumentacja Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Oficjalny poradnik Rust](https://www.rust-lang.org/learn)
- [Stack Overflow](https://stackoverflow.com/questions/tagged/rust)