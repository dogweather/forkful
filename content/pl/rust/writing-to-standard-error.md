---
title:                "Pisanie do standardowego błędu"
html_title:           "Rust: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robimy? 

Pisanie do standardowego błędu (standard error) to proces, w którym programista wysyła błędy lub ostrzeżenia ze swojego programu do standardowego strumienia błędów. Najczęściej robi się to w celu uproszczenia debugowania i znalezienia problemów w kodzie.

## Jak to zrobić? 

Możesz użyć metody standardowej biblioteki Rust, która nazywa się "eprintln!". Poniżej znajduje się przykład z wykorzystaniem tej metody:

```Rust
eprintln!("To jest przykładowy błąd.");
```
 
W efekcie w standardowym strumieniu błędów zostanie wyświetlony tekst "To jest przykładowy błąd."

## Głębsze zagadnienia

Pisanie do standardowego błędu jest często używane podczas debugowania, ponieważ separuje ono błędy od normalnego wyjścia programu, co ułatwia ich znajdowanie. Alternatywną metodą może być wysyłanie błędów do pliku dziennika lub wyświetlanie ich na ekranie. W Rust istnieje również metoda "println!" do wysyłania informacji do standardowego wyjścia.

## Zobacz też 

Dla więcej informacji na temat pisania do standardowego błędu, zobacz dokumentację Rust. Możesz też poczytać o różnicach między standardowym wyjściem a standardowym strumieniem błędów.