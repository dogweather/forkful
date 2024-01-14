---
title:    "Gleam: Tworzenie pliku tymczasowego"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest ważną częścią programowania w Gleam. Czy to do testowania kodu, czy do przechowywania danych, pliki tymczasowe są nieodłącznym elementem projektów. W tym artykule omówimy, dlaczego warto tworzyć pliki tymczasowe przy pomocy języka Gleam.

## Jak to zrobić

Tworzenie plików tymczasowych w języku Gleam jest proste i wygodne. Możemy to zrobić przy pomocy modułu `File.Temporary` oraz funkcji `open_temporary_file`. Poniżej przedstawimy kod przykładowy oraz wynik działania w blokach kodu ```Gleam...```.

```Gleam
import File.Temporary
import File

fn main() {
  // Tworzenie pliku tymczasowego
  let result = File.Temporary.open_temporary_file()
  case result {
    Ok(file) -> {
      // Zapisanie tekstu do pliku
      let text = "Ten tekst zostanie zapisany w pliku tymczasowym"
      File.write(file.handle, text)
      // Odczytanie tekstu z pliku
      let output = File.read_to_string(file.handle)
      // Wyświetlenie tekstu na ekranie
      IO.print(output)
      // Zamknięcie pliku
      File.close(file.handle)
    }
    Err(error) -> IO.print("Wystąpił błąd: {error}")
  }
}
```

Output:
```
Ten tekst zostanie zapisany w pliku tymczasowym
```

## Głębszy wgląd
Tworzenie plików tymczasowych jest nie tylko przydatne, ale również wygodne. Umożliwia to tworzenie testów dla naszego kodu oraz przechowywanie danych w sposób tymczasowy. Jednak należy pamiętać, że pliki tymczasowe są usuwane po zakończeniu działania programu, więc nie powinny być używane do przechowywania danych na stałe.

## Zobacz też

- Dokumentacja modułu `File.Temporary` w języku Gleam: [link](https://gleam.run/modules/file/temporary/)
- Przykłady użycia `File.Temporary`: [link](https://github.com/gleam-lang/gleam_stdlib/blob/master/standard/library/file/temporary/tests/file.temporary.test.gleam)