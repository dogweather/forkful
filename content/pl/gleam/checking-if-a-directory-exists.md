---
title:    "Gleam: Sprawdzanie istnienia katalogu"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy dany katalog istnieje, jest kluczowym elementem programowania w Gleam. Dzięki temu, możemy być pewni, że nasz program będzie działał prawidłowo i nie będzie przerywanych błędami związanymi z nieistniejącymi katalogami. W tym artykule dowiesz się, jak sprawdzić istnienie katalogu w języku Gleam.

## Jak To Zrobić

Aby sprawdzić, czy dany katalog istnieje, możemy użyć wbudowanej funkcji `Dir.exists` w module `gleam/io`. Poniżej znajduje się przykładowy kod, który demonstruje użycie tej funkcji:

```Gleam
import gleam/io

fn main() {
  // Ustawiamy ścieżkę do katalogu, który chcemy sprawdzić
  let path = "ścieżka/do/katalogu"

  // Wywołujemy funkcję Dir.exists i przekazujemy jej ścieżkę jako argument
  let exists = io.Dir.exists(path)

  // Sprawdzamy, czy katalog istnieje
  case exists {
    True -> io.println("Katalog istnieje")
    False -> io.println("Katalog nie istnieje")
  }
}
```

Po uruchomieniu tego kodu, gdy katalog istnieje, zobaczymy na konsoli wiadomość "Katalog istnieje", a gdy nie istnieje - "Katalog nie istnieje".

## Zanurzenie W Temat

Sprawdzanie istnienia katalogu może być przydatne w wielu przypadkach. Na przykład, można go wykorzystać przy tworzeniu plików w określonym katalogu, aby upewnić się, że plik zostanie utworzony w odpowiednim miejscu. Można także skorzystać z tej funkcji w celu bezpiecznego przenoszenia lub usuwania plików. 

Warto także wspomnieć, że funkcja `Dir.exists` zwraca wartość `True` tylko wtedy, gdy podana ścieżka wskazuje na istniejący katalog. W przypadku, gdy podana ścieżka wskazuje na plik lub nie istniejący element, funkcja ta zwróci wartość `False`. 

## Zobacz Również

- [Dokumentacja funkcji Dir.exists w języku Gleam](https://gleam.run/modules/gleam_io/dir.html#exists)
- [Przykłady użycia funkcji Dir.exists](https://github.com/gleam-lang/gleam/blob/master/tests/io_dir_test.gleam)