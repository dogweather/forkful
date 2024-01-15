---
title:                "Sprawdzanie, czy istnieje katalog"
html_title:           "Gleam: Sprawdzanie, czy istnieje katalog"
simple_title:         "Sprawdzanie, czy istnieje katalog"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Kiedy piszesz aplikację w Gleam, ważne jest, aby upewnić się, że katalog, z którego próbujesz pobrać lub zapisać plik, istnieje. W przeciwnym razie Twoja aplikacja może zwrócić błędy lub działania mogą nie być wykonane poprawnie. Dlatego warto wiedzieć, jak sprawdzić, czy dany katalog istnieje w Twoim kodzie.

## Jak to zrobić

Sprawdzenie istnienia katalogu w Gleam jest bardzo proste. Wystarczy użyć funkcji `Gleam.OS.Directory.exists` i podać jako argument ścieżkę do katalogu.

```Gleam
import Gleam.OS.Directory as Directory

let result = Directory.exists("sciezka/do/katalogu")

// Jeśli katalog istnieje, zwróci wartość `ok`. W przeciwnym razie zwróci `Err`.
```

Możesz również wykorzystać ten sam kod w bloku `match` do sprawdzenia, czy katalog istnieje i podejmowania odpowiednich działań w zależności od tego wyniku.

```Gleam
import Gleam.OS.Directory as Directory

match Directory.exists("sciezka/do/katalogu") {
    ok -> "Katalog istnieje!"
    Err -> "Katalog nie istnieje."
}
```

## Głębsze zanurzenie

Funkcja `Gleam.OS.Directory.exists` działa na podstawie API systemu plików Twojego systemu operacyjnego. Dzięki temu jest wysoce niezawodna i da ci pewność, że Twój kod będzie działać zgodnie z oczekiwaniami.

Może się zdarzyć, że chcesz sprawdzić, czy dany katalog nie tylko istnieje, ale także jest to katalog regularny lub symboliczny. W takim przypadku możesz skorzystać z bardziej szczegółowej funkcji `Gleam.OS.Directory.metadata` i sprawdzić rodzaj katalogu za pomocą parametru `type`.

```Gleam
import Gleam.OS.Directory as Directory

match Directory.metadata("sciezka/do/katalogu").type {
    Directory.File -> "To jest plik, a nie katalog!"
    Directory.Directory -> "Katalog istnieje."
}
```

## Zobacz również

- Dokumentacja Gleam do funkcji `Gleam.OS.Directory.exists`: https://gleam.run/api/Gleam.OS.Directory.html#exists
- Dokumentacja Gleam do funkcji `Gleam.OS.Directory.metadata`: https://gleam.run/api/Gleam.OS.Directory.html#metadata