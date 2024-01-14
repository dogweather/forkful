---
title:    "Rust: Odczytywanie pliku tekstowego"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Nie tylko jest to niezwykle przydatna umiejętność, ale także może przynieść wiele satysfakcji i poczucia spełnienia. Jednym z najbardziej popularnych języków programowania jest Rust - nie tylko ze względu na jego prędkość i bezpieczeństwo, ale także ze względu na bogatą bibliotekę, która ułatwia wiele zadań. W tym artykule zajmiemy się czytaniem pliku tekstowego, co jest ważną umiejętnością w programowaniu. 

## Jak to zrobić

W programowaniu istnieje wiele różnych sposobów na czytanie plików tekstowych, ale w tym artykule skupimy się na tym, jak to zrobić w języku Rust. Poniżej przedstawione są przykłady kodu z wyjaśnieniami, które pokażą Ci, jak można to zrobić. Nie zapomnij zaimportować biblioteki `std::fs`, która umożliwi nam pracę z plikami.

```
use std::fs;

fn main() {
    // Przykładowy plik tekstowy "dane.txt"
    let data = fs::read_to_string("dane.txt")
        .expect("Nie udało się odczytać pliku.");

    // Wyświetlenie zawartości pliku w konsoli
    println!("ZAWARTOŚĆ PLIKU:\n{}", data);
}
```

Jeśli wszystko pójdzie dobrze, powinniśmy zobaczyć zawartość pliku tekstowego w konsoli. Proste, prawda? Ale co jeśli chcemy odczytać plik linia po linii lub przetworzyć jego zawartość w inny sposób? W ostatnim przykładzie użyjemy metody `lines()` do odczytania pliku linia po linii i wyświetlenia każdej z nich w osobnej linii konsoli.

```
use std::fs;

fn main() {
    let data = fs::read_to_string("dane.txt")
        .expect("Nie udało się odczytać pliku.");

    // Iterowanie przez linie pliku i wyświetlanie ich w konsoli
    for line in data.lines() {
        println!("{}", line);
    }
}
```

## Głębszy zanurzenie

Powyższe przykłady są tylko wstępem do tematu czytania plików tekstowych w języku Rust. Istnieje wiele innych metod i funkcji, które mogą być przydatne w zależności od naszych celów. Możesz także zapoznać się z bibliotekami takimi jak `serde` lub `csv` do przetwarzania plików tekstowych w bardziej zaawansowany sposób. Ważne jest również zwrócenie uwagi na wyjątki, które mogą być generowane podczas czytania pliku i ich obsługę w naszym kodzie. 

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o czytaniu plików tekstowych w języku Rust, polecamy zapoznać się z oficjalną dokumentacją języka oraz innych artykułów na ten temat:

- [Dokumentacja języka Rust](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Poradnik na stronie Rust by Example](https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html)
- [Tutorial na stronie Rusz na przykładach](https://rsz.io/reading-and-parsing-a-csv-file.html)