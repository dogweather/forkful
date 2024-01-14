---
title:                "Elixir: Odczytywanie pliku tekstowego"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto czytać pliki tekstowe?
W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego codziennego życia. Jednym ze sposobów na tworzenie oprogramowania jest język Elixir. W tej krótkiej publikacji przedstawimy Wam, jak w prosty sposób możecie czytać pliki tekstowe dzięki Elixir i dlaczego warto poznać ten proces.

## Jak to zrobić?
Sam proces czytania plików tekstowych jest bardzo prosty w języku Elixir. Wykorzystując wbudowaną funkcję "File.read", można w łatwy sposób wczytywać pliki z systemu plików. Spróbujmy to zrobić na przykładzie pliku "example.txt":

```Elixir
file = File.read("example.txt")
IO.puts file
```
Po wykonaniu powyższych instrukcji, otrzymamy wynik, czyli zawartość pliku "example.txt".

## Deep Dive
W języku Elixir, funkcja "File.read" zwraca dwa elementy, a są nimi:
  - zawartość wczytanego pliku
  - kod błędu (jeśli wystąpił)

Dzięki temu jesteśmy w stanie sprawdzić, czy wczytywanie pliku przebiegło pomyślnie, czy też wystąpił jakiś błąd. Możemy także ustawić dodatkowe parametry, takie jak np. tryb pracy, co pozwala na większą elastyczność przy operowaniu na plikach tekstowych.

## Zobacz również
- [Dokumentacja języka Elixir](https://elixir-lang.org/)
- [Blog poświęcony językowi Elixir](https://elixirforum.com/)
- [Przykłady zastosowań języka Elixir](https://github.com/h4cc/awesome-elixir)