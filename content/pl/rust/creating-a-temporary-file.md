---
title:    "Rust: Tworzenie pliku tymczasowego"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego
Stworzenie tymczasowego pliku może być niezbędne w wielu programach. Na przykład, gdy musimy tymczasowo przechowywać dane, które będą wykorzystane później w kodzie lub gdy musimy zapisać dane tymczasowo, aby nie przeciążać pamięci.

## Jak to zrobić
Tworzenie tymczasowych plików w języku Rust jest bardzo proste. Możemy użyć modułu "tempfile", który jest już zaimplementowany w standardowej bibliotece języka. W poniższym kodzie pokazane są dwa przykłady tworzenia tymczasowych plików.

```Rust
use std::fs::File;
use tempfile::NamedTempFile;

// Tworzenie pustego tymczasowego pliku
let temp_file = NamedTempFile::new().unwrap();
// Wypisanie ścieżki do utworzonego pliku
println!("{}", temp_file.path().display()); 

// Tworzenie tymczasowego pliku z danymi
let mut data = Vec::new();
data.push(b"Hello, world!");
let temp_file2 = NamedTempFile::new().unwrap();
File::create(temp_file2.path()).unwrap().write_all(&data).unwrap();
// Wypisanie zawartości utworzonego pliku
let mut output = Vec::new();
File::open(temp_file2.path()).unwrap().read_to_end(&mut output).unwrap();
println!("{}", String::from_utf8(output).unwrap());
```

Powyższy kod najpierw importuje moduł "fs::File", który jest odpowiedzialny za operacje na plikach oraz moduł "tempfile::NamedTempFile", dzięki któremu możemy tworzyć tymczasowe pliki. Pierwszy przykład tworzy pusty tymczasowy plik i wypisuje jego ścieżkę, a drugi tworzy tymczasowy plik z danymi i wypisuje jego zawartość. W obu przypadkach możemy zauważyć, że korzystając z modułu "NamedTempFile", nie musimy martwić się o usuwanie tymczasowego pliku - jest on automatycznie usuwany po zakończeniu działania programu.

## Deep Dive
Moduł "tempfile" oferuje nam wiele różnych metod do tworzenia i operowania na tymczasowych plikach. Na przykład, możemy ustawić prefix i suffix dla nazwy tymczasowego pliku lub określić, w jakim katalogu ma być utworzony. Możemy również w łatwy sposób skonfigurować tymczasowy plik, aby miał określony rozmiar lub aby był buforowany w pamięci. Istnieją również inne moduły, takie jak "memmap", który oferuje bardziej zaawansowane operacje na tymczasowych plikach.

## Zobacz także
- [Dokumentacja modułu "tempfile" (język angielski)](https://doc.rust-lang.org/tempfile/tempfile/index.html)
- [Dokumentacja modułu "memmap" (język angielski)](https://docs.rs/memmap/0.7.0/memmap/)
- [Artykuł "Creating Temporary Files in Rust" (język angielski)](https://medium.com/@yfinkelstein/creating-temporary-files-in-rust-360f9f65636#:~:text=Creating%20temporary%20files%20in%20Rust%20is%20very%20simple.&text=Let's%20take%20a%20brief%20tour,the%20standard%20library's%20module%20tempfile%20)