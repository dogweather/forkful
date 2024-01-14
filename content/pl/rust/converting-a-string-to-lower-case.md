---
title:    "Rust: Konwersja ciągu znaków na małe litery"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie tekstu na małe litery może być bardzo przydatne, zwłaszcza jeśli jesteś programistą języka Rust. To pozwala na porównywanie tekstów bez uwzględniania wielkości liter, co ułatwia pracę z tekstem. Dowiedz się, jak to zrobić w sposób efektywny w języku Rust.

## Jak to zrobić

W języku Rust konwertowanie tekstu na małe litery jest łatwe dzięki wbudowanej funkcji `to_lowercase()`. Wystarczy wstawić tekst, który chcesz przekonwertować jako argument tej funkcji, a następnie przypisać wynik do zmiennej lub wyświetlić go bezpośrednio.

```Rust
let text = "JP Programming";
let lowercase_text = String::from(text).to_lowercase();
println!("{}", lowercase_text);
```
**Output:**
jp programming

W powyższym przykładzie wykorzystujemy metodę `from()`, która konwertuje `&str` na `String`, co jest wymagane do użycia funkcji `to_lowercase()`. W przypadku, gdy chcesz przekonwertować bezpośrednio tekst, który już jest w formacie `String`, możesz po prostu wywołać funkcję `to_lowercase()` na tym tekście.

```Rust
let text = String::from("Rust is Fun");
let lowercase_text = text.to_lowercase();
println!("{}", lowercase_text);
```

**Output:**
rust is fun

Pamiętaj, że funkcja `to_lowercase()` nie zmienia oryginalnego tekstu, ale zwraca nowy `String` zawierający przekonwertowany tekst.

## Deep Dive

Konwertowanie tekstu na małe litery może być nieco bardziej skomplikowane, niż się wydaje. Jedną z głównych przeszkód jest obsługa różnych języków i znaków specjalnych. W języku Rust, funkcja `to_lowercase()` obsługuje wszystkie typy znaków, dzięki czemu nie musisz martwić się o te problemy.

Natomiast jeśli chcesz mieć większą kontrolę nad konwersją, istnieje również funkcja `to_ascii_lowercase()`, która przekonwertuje tekst tylko na podstawie znaków ASCII. Jest to przydatne, gdy potrzebujesz prostego porównania tekstów bez uwzględniania wielkości liter.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o konwertowaniu tekstu w języku Rust, warto przeczytać następujące artykuły:

- [Rust String to Int Conversion](https://techalpine.com/rust-string-to-int-conversion/)
- [How to Parse a CSV File in Rust](https://techalpine.com/how-to-parse-a-csv-file-in-rust/)
- [Simple Error Handling in Rust](https://techalpine.com/simple-error-handling-in-rust/)