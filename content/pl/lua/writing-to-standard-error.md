---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu, czyli `stderr`, pozwala oddzielić normalne wyjście programu od komunikatów o błędach. Programiści używają tego, żeby ułatwić sobie debugowanie i logowanie, pozostawiając wyjście programu (`stdout`) czystym do dalszego przetwarzania.

## Jak to zrobić:
```Lua
-- Przykładowy kod w Lua
io.stderr:write("To jest komunikat błędu\n")

-- Alternatywnie, używając funkcji print
local oldstd = io.output()
io.output(io.stderr)
print("To jest inny komunikat błędu")
io.output(oldstd)
```

Przykładowe wyjście:
```
To jest komunikat błędu
To jest inny komunikat błędu
```

## Głębsze spojrzenie
W przeszłości `stderr` był używany głównie do raportowania błędów w terminalach. Alternatywą jest przekierowanie komunikatów błędów do pliku lub użycie biblioteki logującej. Lua traktuje `stderr` jako oddzielny strumień i oferuje prostą funkcję `io.stderr:write()`, by do niego pisać bezpośrednio, co jest odpowiednikiem pisania do `STDOUT` za pomocą `print()`. 

## Zobacz również
- Lua `io` library documentation: [http://www.lua.org/manual/5.4/manual.html#6.8](http://www.lua.org/manual/5.4/manual.html#6.8)
- Unix philosophy on stdout and stderr: [https://en.wikipedia.org/wiki/Unix_philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)
