---
title:                "Gleam: Odczytanie pliku tekstowego"
simple_title:         "Odczytanie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś/aś przeczytać plik tekstowy w swoim programie? Jeśli tak, to wiesz jak ważne jest to umiejętność w świecie programowania. Niezależnie od tego, czy chcesz przetwarzać duże zbiory danych, czy tylko sprawdzić zawartość prostego pliku, opcja ta jest niezbędna w wielu sytuacjach. W tym artykule dowiesz się, jak w prosty sposób czytać pliki tekstowe w języku programowania Gleam.

## Jak to zrobić

### Początek

Aby móc czytać pliki tekstowe w języku Gleam, musimy najpierw utworzyć odpowiednią funkcję, która będzie zajmować się tym zadaniem. Na początek utwórzmy funkcję `read_file`, która jako argument będzie przyjmować ścieżkę do pliku.

```Gleam
fn read_file(path) {
  // kod do odczytania pliku
}
```

### Otwarcie pliku

Korzystając z funkcji `File.open`, możemy otworzyć plik o podanej ścieżce i zwrócić obiekt pliku, który później będziemy mogli wykorzystać.

```Gleam
fn read_file(path) {
  let file = File.open(path)
}
```

### Odczytywanie danych

Następnie musimy odczytać zawartość pliku i przekazać ją do zmiennej. W tym celu użyjemy funkcji `File.read` oraz przypiszemy wynik do nowej zmiennej `data`.

```Gleam
fn read_file(path) {
  let file = File.open(path)
  let data = File.read(file)
}
```

### Zamknięcie pliku

Na zakończenie, musimy pamiętać o zamknięciu pliku, aby nie zajmował on niepotrzebnie zasobów naszego programu. W tym celu wykorzystamy funkcję `File.close` i przekażemy do niej obiekt pliku jako argument.

```Gleam
fn read_file(path) {
  let file = File.open(path)
  let data = File.read(file)
  File.close(file)
}
```

### Przykład użycia

Teraz, gdy mamy już funkcję `read_file` gotową, możemy jej użyć w naszym programie. Przykładem może być odczytywanie zawartości pliku tekstowego i wyświetlenie jej na ekranie.

```Gleam
let file_path = "ciekawy_plik.txt"

let file_data = read_file(file_path)

IO.print(file_data)
```

W powyższym przykładzie, najpierw zdefiniowaliśmy zmienną `file_path` przechowującą ścieżkę do pliku. Następnie, używając funkcji `read_file`, odczytaliśmy jego zawartość i przypisaliśmy do zmiennej `file_data`. Na koniec, przy pomocy funkcji `IO.print` wyświetliliśmy odczytaną zawartość na ekranie.

## Głębsze wyjaśnienie

Oprócz odczytywania całych plików, istnieje również możliwość odczytywania ich wiersz po wierszu. W tym celu możemy skorzystać z funkcji `File.read_line`, która będzie czytać kolejne wiersze pliku i zwracać je jako wynik.

```Gleam
fn read_file(path) {
  let file = File.open(path)
  let line = File.read_line(file)

  while let Some(data) = line {
    // zrobić coś z danymi, np. wydrukować je na ekranie
    IO.print(data)
    line = File.read_line(file)
  }

  File.close(file)
}
```

W powyższym przykładzie użyliśmy pętli `while` do odczytywania kolejnych wierszy pliku za pomocą funkcji `File.read_line`. Takie pode