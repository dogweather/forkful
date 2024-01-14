---
title:    "Gleam: Tworzenie pliku tekstowego"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest ważnym elementem każdego języka programowania. Pozwala ono na zapisywanie danych w czytelnej formie i jest niezbędne do wielu zadań, takich jak tworzenie plików konfiguracyjnych, zapisywanie danych użytkownika i wiele innych. W tym artykule dowiesz się, jak korzystać z języka programowania Gleam, aby pisać pliki tekstowe i jakie korzyści może to przynieść.

## Jak to zrobić

Pisanie plików tekstowych w języku Gleam jest bardzo proste. Wystarczy użyć funkcji `File.write` i podać nazwę pliku oraz zawartość, którą chcesz zapisać. Poniżej znajduje się przykładowy kod, który zapisze tekst "Witaj świecie!" do pliku o nazwie "hello.txt":

```Gleam
File.write("hello.txt", "Witaj świecie!")
```

Po wykonaniu tego kodu, w katalogu, w którym znajduje się plik z programem, powinien pojawić się plik "hello.txt" zawierający tekst "Witaj świecie!".

Możesz również użyć funkcji `File.append`, aby dopisywać zawartość do istniejącego już pliku, zamiast nadpisywać go w całości. Przykładowy kod będzie wyglądał tak:

```Gleam
File.append("hello.txt", "To jest kolejna linijka tekstu.")
```

Teraz plik "hello.txt" będzie zawierał dwa wpisy tekstu.

## Deep Dive

W języku Gleam istnieje wiele funkcji do tworzenia, odczytywania i modyfikowania plików tekstowych. Możesz zapisywać dane w różnych formatach, takich jak JSON czy CSV, a także używać różnych kodowań, na przykład UTF-8. Możesz również wczytywać dane z pliku i przetwarzać je w swoim programie.

Jedną z przydatnych funkcji jest `File.read`, która pozwala na odczytywanie zawartości pliku tekstowego. Przykładowy kod będzie wyglądał tak:

```Gleam
// Odczytaj zawartość pliku "hello.txt"
let zawartosc = File.read("hello.txt")

// Wyświetl zawartość na ekranie
IO.println(zawartosc)
```

Wynikiem będzie wyświetlenie zawartości pliku "hello.txt", czyli "Witaj świecie! To jest kolejna linijka tekstu.".

## Zobacz też

- Dokumentacja języka Gleam: https://gleam.run/documentation/
- Przykładowe projekty w języku Gleam: https://github.com/gleam-lang/awesome-gleam