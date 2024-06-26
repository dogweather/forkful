---
date: 2024-01-20 17:54:07.059783-07:00
description: "How to: Elixir korzysta z beamowego (wirtualna maszyna Erlanga) modelu\
  \ obs\u0142ugi plik\xF3w. To pochodna erlangowego sposobu czytania plik\xF3w \u2013\
  \ niezawodnego i\u2026"
lastmod: '2024-04-05T21:53:36.500328-06:00'
model: gpt-4-1106-preview
summary: "Elixir korzysta z beamowego (wirtualna maszyna Erlanga) modelu obs\u0142\
  ugi plik\xF3w."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to:
```elixir
# Otwieramy i czytamy plik linia po linii
File.stream!("plik.txt") 
|> Enum.each(fn line -> IO.puts(line) end)

# Wynik:
# Pierwsza linia pliku.
# Druga linia pliku.
```

```elixir
# Wczytujemy całą zawartość pliku na raz
{ok, content} = File.read("plik.txt")
IO.puts(content)

# Wynik:
# Cała zawartość pliku wypisana jednym ciągiem.
```

## Deep Dive
Elixir korzysta z beamowego (wirtualna maszyna Erlanga) modelu obsługi plików. To pochodna erlangowego sposobu czytania plików – niezawodnego i wydajnego. Alternatywą jest otwarcie pliku za pomocą `File.open/2` i ręczne iterowanie. W przypadku dużych plików lepiej użyć `File.stream!/3`, by operować na strumieniach i zmniejszyć zużycie pamięci.

## See Also
- Dokumentacja do `File` modułu Elixir: https://hexdocs.pm/elixir/File.html
- Przewodnik do strumieni i leniwego przetwarzania w Elixirze: https://elixirschool.com/pl/lessons/basics/streams/
