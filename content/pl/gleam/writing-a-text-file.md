---
title:                "Tworzenie pliku tekstowego"
html_title:           "Gleam: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego to proces, w którym programiści tworzą plik zawierający tekstowe informacje. Jest to powszechna praktyka w projektowaniu oprogramowania, ponieważ pozwala na zapisywanie danych, takich jak ustawienia, wyniki lub dane wejściowe, które mogą być wykorzystane później przez program.

## Jak to zrobić:
### Wysyłanie tekstu bezpośrednio do pliku
Gleam posiada wbudowany moduł `File` do zarządzania plikami tekstowymi. Aby utworzyć nowy plik i zapisać do niego tekst, wystarczy wywołać funkcję `File.write`, podając ścieżkę do pliku i tekst, który chcemy zapisać.

```Gleam
import File

File.write("ścieżka/do/pliku.txt", "To jest tekst, który zostanie zapisany do pliku!")
```

### Dodawanie tekstu do istniejącego pliku
Aby dodać nowy tekst do istniejącego pliku, można użyć funkcji `File.append`, która działa w podobny sposób do `File.write`.

```Gleam
import File

File.append("ścieżka/do/pliku.txt", "To jest kolejna linijka tekstu, która zostanie dodana!")
```

### Odczytywanie tekstu z pliku
Jeśli chcemy odczytać zawartość pliku tekstowego i zapisać ją do zmiennej, można użyć funkcji `File.read`.

```Gleam
import File

let text = File.read("ścieżka/do/pliku.txt")
```

```text
// W przypadku pliku zawierającego tekst: 
To jest tekst, który zostanie zapisany do pliku!
To jest kolejna linijka tekstu, która zostanie dodana!
// Wartość zmiennej `text` będzie:
"To jest tekst, który zostanie zapisany do pliku!\nTo jest kolejna linijka tekstu, która zostanie dodana!"
```

## Deep Dive:
Historia plików tekstowych sięga początków komputerów i jest jednym z najważniejszych sposobów przechowywania i udostępniania informacji. Alternatywami dla plików tekstowych mogą być bazy danych lub pliki binarne, ale w wielu sytuacjach są one mniej intuicyjne lub trudniejsze w użyciu.

Implementacja modułu `File` w Gleamie wykorzystuje standardową bibliotekę języka Erlang, co gwarantuje stabilne i wydajne działanie.

## Zobacz też:
- Dokumentacja modułu `File` w Gleamie: https://gleam.run/lib/std#file
- Wprowadzenie do programowania w Gleamie (w języku polskim): https://gleam.run/book/pl/