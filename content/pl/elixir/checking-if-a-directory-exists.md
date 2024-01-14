---
title:                "Elixir: Sprawdzanie, czy istnieje katalog"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy istnieje katalog, jest ważną umiejętnością dla każdego programisty Elixir. Dzięki temu narzędziu możemy upewnić się, że nasz program działa poprawnie i jest odporny na nieoczekiwane błędy. W tym artykule dowiesz się, jak sprawdzić istnienie katalogu w języku Elixir.

## Jak To Zrobić

Sprawdzenie istnienia katalogu w Elixir jest bardzo proste. Wystarczy użyć funkcji `File.dir` i podać nazwę katalogu jako argument. Przykładowy kod poniżej pokaże, jak to zrobić:

```elixir
File.dir?("nazwa_katalogu")
```

Jeśli katalog istnieje, funkcja zwróci `true`, w przeciwnym wypadku zwróci `false`. Sprawdzenie można również wykonać w bardziej bezpośredni sposób, używając funkcji `File.ls` i sprawdzając, czy nazwa katalogu jest na liście plików. Przykładowy kod poniżej:

```elixir
"nazwa_katalogu" in File.ls(".")
```

W obu przypadkach używa się nazwy oraz ścieżki katalogu w postaci stringa, jednak warto pamiętać, że w przypadku użycia `File.dir?` ścieżka musi być zaczynająca się od kropki.

## Deep Dive

Sprawdzenie istnienia katalogu jest często wykorzystywane w testowaniu programów, aby upewnić się, że wszystkie niezbędne katalogi istnieją przed uruchomieniem aplikacji. Można również wykorzystać tę funkcję w celu dodatkowej ochrony przed błędami, sprawdzając, czy katalog jest pusty lub czy zawiera odpowiednie pliki.

Warto również wspomnieć, że funkcja `File.dir?` zwróci `false` jeśli nie uda się otworzyć katalogu, na przykład w przypadku braku odpowiednich uprawnień. Dzięki temu możemy uniknąć potencjalnych błędów w naszym programie.

## Zobacz Również

- [Dokumentacja Elixir na temat sprawdzania istnienia plików](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Poradnik Elixir na temat operacji na plikach](https://elixir-lang.org/getting-started/io-and-the-file-system.html)