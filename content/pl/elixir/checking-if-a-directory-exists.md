---
title:    "Elixir: Sprawdzanie czy istnieje folder"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie pisania aplikacji w Elixirze chcemy upewnić się, czy dany katalog istnieje przed wykonaniem operacji na plikach wewnątrz niego. Warto więc poznać, jak w prosty sposób można sprawdzić, czy dany katalog istnieje w naszym systemie plików.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje w Elixirze jest bardzo proste. Należy skorzystać z funkcji `:file.cwd/0`, która będzie zwracać aktualną ścieżkę do katalogu roboczego, i porównać ją z poszukiwanym katalogiem. Kod będzie wyglądał następująco:

```
# Wczytanie biblioteki :file
:file.cwd()

# Sprawdzenie, czy katalog "images" znajduje się w aktualnym katalogu roboczym
if :file.cwd() == "images" do
  IO.puts "Katalog 'images' istnieje."
else
  IO.puts "Katalog 'images' nie istnieje."
end
```

Output dla powyższego kodu będzie wyglądał następująco:

```
Katalog 'images' nie istnieje.
```

## Głębszy wgląd

Warto również wiedzieć, że funkcja `:file.cwd/0` zwraca ciąg znaków, a niebezpiecznie jest polegać na konkretnych ścieżkach w kodzie. Lepiej jest wykorzystać funkcję `:file.expand_path/1`, aby przekonwertować ścieżkę na absolutną. W ten sposób nasz kod będzie bardziej niezawodny.

```
# Wczytanie biblioteki :file
:file.cwd()

# Przekonwertowanie ścieżki na absolutną
:file.expand_path("images")

# Sprawdzenie, czy katalog "images" znajduje się w aktualnym katalogu roboczym
if :file.cwd() == "images" do
  IO.puts "Katalog 'images' istnieje."
else
  IO.puts "Katalog 'images' nie istnieje."
end
```

## Zobacz również

- [Dokumentacja Elixir: :file.cwd/0](https://hexdocs.pm/elixir/File.html)
- [Dokumentacja Elixir: :file.expand_path/1](https://hexdocs.pm/elixir/File.html#expand_path/1)