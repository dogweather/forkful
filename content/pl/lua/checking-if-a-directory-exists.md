---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:52.389179-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzenie, czy katalog istnieje, to proces weryfikacji obecności folderu w systemie plików. Programiści wykonują to, by uniknąć błędów podczas tworzenia plików, zapisu danych, czy przetwarzania ścieżek - jeśli katalog nie istnieje, działanie może się nie powieść.

## Jak to zrobić:

Sprawdzanie istnienia katalogu w Lua wymaga użycia funkcji zewnętrznych, ponieważ standardowe API nie oferuje takiej funkcjonalności. Poniżej znajdziesz przykład z wykorzystaniem standardowej biblioteki `os` oraz biblioteki `lfs` (Lua File System).

```Lua
local lfs = require('lfs')

-- Sprawdzenie przy użyciu lfs
function directory_exists(path)
    local attributes = lfs.attributes(path)
    return attributes and attributes.mode == "directory"
end

-- Sprawdzenie przy użyciu os.execute i komendy systemowej (działa głównie na systemach typu Unix)
function directory_exists_with_os(path)
    local command = string.format('cd %s 2>/dev/null', path)
    local success = os.execute(command)
    return success == true
end

print(directory_exists("/tmp")) -- Wynik: prawda lub fałsz
print(directory_exists_with_os("/tmp")) -- Wynik: prawda lub fałsz
```

Pamiętaj, że wykorzystanie `os.execute` może być mniej przenośne i bezpieczne, dlatego częściej zaleca się użycie `lfs`.

## Wnikliwa analiza

Sprawdzenie istnienia katalogu jest istotne przy manipulowaniu plikami; podstawowe API Lua nie obsługuje tego bezpośrednio, więc trzeba polegać na bibliotekach zewnętrznych, takich jak `lfs`, którą można zainstalować za pomocą menedżera pakietów LuaRocks:

```bash
luarocks install luafilesystem
```

`lfs` zapewnia bogatszy interfejs do pracy z systemem plików, dobrze integruje się z Lua i jest szeroko używana. Alternatywnie, można użyć komend systemowych, ale to podejście może być platformozależne i narażone na zagadnienia bezpieczeństwa związane z wstrzykiwaniem poleceń.

Historia: funkcje takie jak te dostarczane przez `lfs` kiedyś były częścią więcej złożonych skryptów przed pojawieniem się dedykowanych bibliotek.

## Zobacz także

- Lua File System (LFS):
  [http://keplerproject.github.io/luafilesystem](http://keplerproject.github.io/luafilesystem)
- LuaRocks - Menedżer Pakietów:
  [https://luarocks.org/](https://luarocks.org/)
- Lua 5.4 Reference Manual (w szczególności sekcja o `os` i `io`):
  [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
