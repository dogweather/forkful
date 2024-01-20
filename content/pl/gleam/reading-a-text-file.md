---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie plików tekstowych polega na odczytywaniu danych zapisanych w formacie tekstowym. Programiści robią to, aby uzyskać dostęp do przechowywanych danych, które potem mogą być wykorzystane w aplikacjach.

## Jak to zrobić:
Oto przykład, jak można odczytać plik tekstowy w Gleam:

```gleam
let main = fn() {
    let result = gleam::file::read_file("plik.txt")
        .expect("Nie udało się odczytać pliku");

    case result {
        Ok(content) -> print(content),
        Error(err) -> print(err)
    };
};
```

Gdy uruchomisz ten kod, wydrukuje on zawartość pliku "plik.txt" lub błąd, jeśli nie udało się go odczytać.

## Głębsze zrozumienie
Odczytywanie plików tekstowych to podstawowa umiejętność, niezależnie od języka programowania. Początkowo, w erze kart perforowanych, odczyt danych był ograniczony do czytania informacji z fizycznych kart.

W dzisiejszych czasach, mamy wiele innych możliwości, takich jak data streaming, API i bazy danych. Mimo to, odczyt plików tekstowych pozostaje istotnym narzędziem, szczególnie jeżeli chodzi o konfigurację, logi czy dane w formacie CSV.

W gleam::file::read_file, wiele rzeczy dzieje się "pod spodem". Funkcja korzysta z modułu IO Rusta do odczytu pliku, a następnie konwertuje te dane na ciąg typu String.

## Zobacz także
Zajrzyj do oficjalnej dokumentacji Gleam, aby nauczyć się więcej:
[Gleam IO moduł](https://docs.gleam.run/stdlib/gleam.file/) - Informacje o modułach IO w Gleam.

[Rust IO Dokumentacja](https://doc.rust-lang.org/std/io/) - Aby dowiedzieć się więcej o tym, jak Gleam korzysta z Rust IO.

[Tutorial o Gleam](https://gleam.run/getting-started/) - Szczegółowy tutorial o programowaniu w Gleam.