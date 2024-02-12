---
title:                "Pisanie pliku tekstowego"
aliases:
- /pl/lua/writing-a-text-file/
date:                  2024-02-03T19:28:51.325773-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie pliku tekstowego"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zapisywanie do pliku tekstowego w Lua polega na utworzeniu lub otwarciu pliku w trybie do zapisu, a następnie używaniu operacji na plikach do wstawiania tekstu. Jest to podstawowa operacja dla zadań takich jak logowanie, przechowywanie danych czy zarządzanie konfiguracją, umożliwiająca programom trwałe zapisywanie danych między sesjami.

## Jak to zrobić:

Praca z plikami pod kątem zapisu w Lua jest prosta. Zwykle używa się funkcji `io.open()`, aby otworzyć (lub utworzyć) plik, określając tryb działania -- w tym przypadku `"w"` dla zapisu. Jeśli plik nie istnieje, zostaje utworzony; jeśli istnieje, jego zawartość jest nadpisywana. Ważne jest, aby zamknąć plik po zapisie, aby zapewnić prawidłowe zapisanie danych i zwolnienie zasobów.

Oto prosty przykład, który zapisuje ciąg znaków do pliku o nazwie "example.txt":

```lua
-- Otwarcie pliku w trybie do zapisu
local file, err = io.open("example.txt", "w")

-- Sprawdzenie błędów przy otwieraniu pliku
if not file then
    print("Nie można otworzyć pliku: ", err)
    return
end

-- Tekst do zapisania w pliku
local text = "Hello, Lua!"

-- Zapis tekstu do pliku
file:write(text)

-- Zamknięcie pliku
file:close()

print("Plik został pomyślnie zapisany.")
```

**Przykładowe wyjście:**
```
Plik został pomyślnie zapisany.
```

**Zapisywanie wielu linii:**

Aby zapisać wiele linii, można użyć `\n` dla nowych linii w ciągu tekstowym lub wywołać `file:write` wiele razy.

```lua
local lines = {
    "Pierwsza linia.",
    "Druga linia.",
    "Trzecia linia."
}

local file = assert(io.open("multiple_lines.txt", "w"))

for _, line in ipairs(lines) do
    file:write(line, "\n")
end

file:close()

print("Wiele linii zostało pomyślnie zapisanych.")
```

**Przykładowe wyjście:**
```
Wiele linii zostało pomyślnie zapisanych.
```

**Korzystanie z bibliotek innych firm:**

Chociaż standardowa biblioteka Lua jest całkiem zdolna, do bardziej złożonych operacji na plikach można rozważyć użycie biblioteki firm trzecich, takiej jak *Penlight*. Penlight rozszerza standardowe operacje plikowe Lua i oferuje łatwiejsze sposoby pracy z plikami i katalogami.

Po zainstalowaniu Penlight można zapisać do pliku w następujący sposób:

```lua
local pl = require "pl"
local path = require "pl.path"
local file = require "pl.file"

-- Tekst do zapisu
local text = "Hello, Penlight!"

-- Korzystanie z Penlight do zapisu do pliku
local result, err = file.write("hello_penlight.txt", text)

if not result then
    print("Błąd podczas zapisywania pliku: ", err)
else
    print("Plik został pomyślnie zapisany za pomocą Penlight.")
end
```

**Przykładowe wyjście:**
```
Plik został pomyślnie zapisany za pomocą Penlight.
```
