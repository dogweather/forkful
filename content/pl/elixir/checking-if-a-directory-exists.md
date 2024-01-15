---
title:                "Sprawdzanie czy katalog istnieje."
html_title:           "Elixir: Sprawdzanie czy katalog istnieje."
simple_title:         "Sprawdzanie czy katalog istnieje."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedykolwiek, czy istnieje określony katalog na twoim komputerze? Może chcesz upewnić się, że plik, który chcesz utworzyć, nie zastąpi istniejącego katalogu? W tym artykule omówimy, dlaczego warto sprawdzać, czy katalog istnieje, oraz jak to zrobić w języku Elixir.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje, w języku Elixir jest bardzo proste. Wystarczy użyć funkcji ```File.cwd``` do pobrania obecnej lokalizacji, a następnie funkcji ```File.exists?``` do sprawdzenia, czy dany katalog istnieje. 

```elixir
current_dir = File.cwd() # pobranie obecnej lokalizacji
existing_dir = "projekty" # nazwa istniejącego katalogu
new_dir = "nowy_projekt" # nazwa nowego katalogu

File.exists?(current_dir <> "/" <> existing_dir) # zwróci true, jeśli "projekty" istnieje
File.exists?(current_dir <> "/" <> new_dir) # zwróci false, jeśli "nowy_projekt" nie istnieje 
```

Jedną z rzeczy, na których warto zwrócić uwagę, jest fakt, że funkcja ```File.exists?``` przyjmuje jako argument ścieżkę do pliku lub katalogu w postaci ciągu znaków. Dlatego też używamy ```<>``` do połączenia nazwy katalogu z obecną lokalizacją. 

## Deep Dive

Podczas sprawdzania, czy dany katalog istnieje, warto mieć na uwadze kilka rzeczy. Po pierwsze, funkcja ```File.cwd``` zwraca ścieżkę w formacie ```absolutny```, a funkcja ```File.exists?``` oczekuje ścieżki w formacie ```relatywnym```. Dlatego też używamy ```<>``` w przykładzie, aby dodać separator ```/```, który jest wymagany w formacie relatywnym. 

Kolejną ważną rzeczą jest fakt, że funkcja ```File.exists?``` sprawdza zarówno pliki, jak i katalogi. Jeśli chcemy ograniczyć sprawdzanie tylko do katalogów, możemy użyć funkcji ```File.dir?```.

```elixir
File.dir?(current_dir <> "/" <> existing_dir) # zwróci true, jeśli "projekty" istnieje i jest katalogiem
```

Sposób, w jaki sprawdzamy, czy katalog istnieje, może również zależeć od systemu operacyjnego. Na przykład na systemie Windows używamy separatora ```\```, a nie ```/```, więc warto uwzględnić to w naszym kodzie. 

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcjach, które przydadzą się podczas pracy z plikami i katalogami w języku Elixir, zobacz te zasoby: 

- [Dokumentacja o funkcji File.cwd](https://hexdocs.pm/elixir/File.html#cwd/0)
- [Dokumentacja o funkcji File.exists?](https://hexdocs.pm/elixir/File.html#exists?/1)
- [Dokumentacja o funkcji File.dir?](https://hexdocs.pm/elixir/File.html#dir?/1)