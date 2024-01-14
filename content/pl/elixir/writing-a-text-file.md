---
title:                "Elixir: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać pliki tekstowe?

Pisanie plików tekstowych jest nieodłączną częścią programowania w Elixirze. Jest to skuteczny sposób na przechowywanie danych w strukturach niezależnych od komputera. Pozwala to na łatwiejsze przenoszenie i udostępnianie danych.

## Jak to zrobić?

Poniżej znajdziesz przykłady kodu w języku Elixir wraz z wynikami w postaci komentarzy w blokach "```Elixir ... ```". W ten sposób, nauczysz się pisać pliki tekstowe w Elixirze w prosty i zrozumiały sposób.

```Elixir
# Otwieramy plik do zapisu
file = File.open("plik.txt", [:write])

# Zapisujemy tekst do pliku
IO.write(file, "Cześć! Jestem plikiem tekstowym napisanym w Elixirze.")

# Zamykamy plik
File.close(file)

# Otwieramy plik do odczytu
file = File.open("plik.txt", [:read])

# Wyświetlamy zawartość pliku
IO.read(file)

# Zamykamy plik
File.close(file)
```

Wynik:

```
"Cześć! Jestem plikiem tekstowym napisanym w Elixirze."
```

## Głębszy zanurzenie

Pisanie plików tekstowych w Elixirze ma wiele zalet. Można w nich przechowywać różnego rodzaju dane, np. teksty, liczby, zapisywanie wyników funkcji, a także tworzenie nowych plików lub nadpisywanie istniejących. Ponadto, dzięki wykorzystaniu struktur danych w Elixirze, można łatwo i szybko przetwarzać informacje zapisane w plikach.

Dzięki wbudowanym funkcjom w języku Elixir, operowanie na plikach tekstowych jest nie tylko proste, ale również wydajne. Ponadto, jest to niezbędna umiejętność dla każdego programisty Elixira, który chce tworzyć solidne i funkcjonalne aplikacje.

## Zobacz również

- [Dokumentacja Elixir - Praca z plikami](https://hexdocs.pm/elixir/File.html)
- [Poradnik dla początkujących w Elixirze](https://medium.com/@piotrgiedzi/unboxing-elixir-poradnik-dla-pocz%C4%85tkuj%C4%85cych-49757d15c02b)
- [10 powodów, dla których warto zacząć programować w Elixirze](https://medium.com/@owolox/10-ways-elixir-nerds-use-elixir-to-hack-their-lives-f9188296adb8)