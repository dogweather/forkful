---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:05.264390-07:00
description: "Jak? Lua nie posiada wbudowanego wsparcia dla YAML, ale mo\u017Cna pracowa\u0107\
  \ z plikami YAML, u\u017Cywaj\u0105c bibliotek firm trzecich, takich jak `lyaml`.\
  \ Ta biblioteka\u2026"
lastmod: '2024-03-13T22:44:35.560442-06:00'
model: gpt-4-0125-preview
summary: "Lua nie posiada wbudowanego wsparcia dla YAML, ale mo\u017Cna pracowa\u0107\
  \ z plikami YAML, u\u017Cywaj\u0105c bibliotek firm trzecich, takich jak `lyaml`."
title: Praca z YAML
weight: 41
---

## Jak?
Lua nie posiada wbudowanego wsparcia dla YAML, ale można pracować z plikami YAML, używając bibliotek firm trzecich, takich jak `lyaml`. Ta biblioteka umożliwia kodowanie i dekodowanie danych YAML z Lua. Najpierw musisz zainstalować `lyaml` za pomocą LuaRocks, managera pakietów Lua:

```bash
luarocks install lyaml
```

### Dekodowanie YAML:
Załóżmy, że masz następującą zawartość YAML w pliku o nazwie `config.yaml`:

```yaml
bazy_danych:
  host: localhost
  port: 3306
  nazwa_użytkownika: użytkownik
  hasło: hasło
```

Możesz zdekodować ten plik YAML do tablicy Lua za pomocą następującego kodu:

```lua
local yaml = require('lyaml')
local plik = io.open("config.yaml", "r")
local zawartość = plik:read("*all")
plik:close()

local dane = yaml.load(zawartość)
for k,v in pairs(dane.bazy_danych) do
  print(k .. ": " .. v)
end
```

Po uruchomieniu tego skryptu, jego wynik powinien być:

```output
host: localhost
port: 3306
nazwa_użytkownika: użytkownik
hasło: hasło
```

### Kodowanie YAML:
Aby zakodować tablice Lua do formatu YAML, używasz funkcji `dump` dostarczonej przez `lyaml`. Rozważając, że chcesz stworzyć reprezentację YAML następującej tablicy Lua:

```lua
local dane = {
  strona_internetowa = {
    nazwa = "Przykład",
    właściciel = "Jane Doe",
    metadane = {
      data_utworzenia = "2023-01-01",
      tagi = {"blog", "personalny", "lua"}
    }
  }
}

local yaml = require('lyaml')
local dane_yaml = yaml.dump({dane})
print(dane_yaml)
```

Wyjście YAML będzie:

```yaml
- strona_internetowa:
    metadane:
      data_utworzenia: '2023-01-01'
      tagi: [blog, personalny, lua]
    nazwa: Przykład
    właściciel: Jane Doe
```

Podążając za tymi wzorcami, programiści Lua mogą efektywnie zarządzać danymi YAML dla różnorodnych aplikacji. Te operacje z YAML są kluczowe dla rozwoju wszechstronnych aplikacji Lua, które wchodzą w interakcje z innymi częściami systemu lub bezpośrednio z innymi systemami.
