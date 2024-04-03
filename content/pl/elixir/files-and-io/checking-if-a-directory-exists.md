---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:15.897727-07:00
description: "Jak to zrobi\u0107: Standardowa biblioteka Elixira oferuje prosty spos\xF3\
  b na sprawdzenie, czy katalog istnieje, za po\u015Brednictwem modu\u0142u `File`.\
  \ Oto jak mo\u017Cesz z\u2026"
lastmod: '2024-03-13T22:44:35.058821-06:00'
model: gpt-4-0125-preview
summary: "Standardowa biblioteka Elixira oferuje prosty spos\xF3b na sprawdzenie,\
  \ czy katalog istnieje, za po\u015Brednictwem modu\u0142u `File`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Standardowa biblioteka Elixira oferuje prosty sposób na sprawdzenie, czy katalog istnieje, za pośrednictwem modułu `File`. Oto jak możesz z tego skorzystać:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Katalog istnieje!"
else
  IO.puts "Katalog nie istnieje."
end
```

Przykładowy wynik, zakładając, że katalog nie istnieje:
```
Katalog nie istnieje.
```

Dla bardziej zaawansowanych interakcji z systemem plików, w tym sprawdzania istnienia katalogu, możesz rozważyć użycie bibliotek stron trzecich, takich jak `FileSystem`. Chociaż standardowe możliwości Elixira są wystarczające w wielu przypadkach, `FileSystem` może oferować bardziej subtelne sterowanie i informacje zwrotne dla złożonych aplikacji. Jednak dla podstawowej potrzeby sprawdzenia, czy katalog istnieje, zaleca się zazwyczaj korzystanie z natywnego modułu `File`, ponieważ jest on łatwo dostępny i nie wymaga żadnych zewnętrznych zależności.
