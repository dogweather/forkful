---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- pl/elixir/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:15.897727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w Elixirze, polega na weryfikacji obecności katalogu pod określoną ścieżką w systemie plików. Programiści robią to, aby upewnić się, że mogą bezpiecznie czytać z katalogu, zapisywać do niego lub wykonywać na nim operacje bez napotkania błędów spowodowanych jego brakiem.

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
