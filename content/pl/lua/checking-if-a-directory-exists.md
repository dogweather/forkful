---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Lua: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, to proces używany przez programistów do potwierdzenia, czy dany katalog jest dostępny w systemie plików. Jest to kluczowe dla zapobiegania błędom, które mogą wystąpić, gdy program próbuje uzyskać dostęp do katalogu, który nie istnieje.

## Jak to zrobić:

Używamy funkcji `lfs.attributes` z biblioteki `lfs` (LuaFileSystem) w Lua. Funkcja ta zwraca tabelę informacji o katalogu lub pliku, jeśli istnieje. Oto przykładowy kod:

```Lua
local lfs = require('lfs')

function DirectoryExists(path)
    local attr = lfs.attributes(path)
    return (attr ~= nil) and (attr.mode == 'directory')
end

print(DirectoryExists('/moj/katalog'))  -- zamień '/moj/katalog' na ścieżkę do rzeczywistego katalogu 
```

Utwórz ten skrypt i uruchom go. Jeśli wydrukowane jest `true`, oznacza to, że katalog istnieje. W przeciwnym razie, jeśli jest `false`, katalog nie istnieje.

## Dogłębna analiza

Funkcja `lfs.attributes` jest częścią biblioteki LuaFileSystem, która jest zewnętrznym rozszerzeniem Lua dla operacji na plikach i katalogach. Ta funkcja była dostępna od Lua 5.1, co pokazuje jej dojrzałość i niezawodność.

Alternatywą dla lfs jest użycie składni `os.execute` z wbudowanym poleceniem systemowym, ale generalnie jest niezalecane ze względu na zależności platformowe i potencjalne zagrożenia bezpieczeństwa.

Implementacja `lfs.attributes` uwzględnia różne atrybuty katalogu lub pliku, takie jak czas modyfikacji, rozmiar i typ. W tym przypadku koncentrujemy się tylko na `mode`, aby sprawdzić, czy ścieżka jest katalogiem.

## Zobacz też

Zainteresowanych czytelników odsyłam do oficjalnej dokumentacji biblioteki LuaFileSystem na stronie: https://keplerproject.github.io/luafilesystem/, gdzie znajdziesz więcej informacji na temat różnych funkcji i ich użycia. Jeśli chcesz rozwijać swoją wiedzę na temat Lua, polecam stronę: https://www.lua.org/docs.html.