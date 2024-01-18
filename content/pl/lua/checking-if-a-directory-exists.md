---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Lua: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Sprawdzanie, czy katalog istnieje, to proces, w którym programista sprawdza, czy dany katalog jest obecny w systemie plików. Jest to ważna czynność, ponieważ programy często potrzebują dostępu do określonych plików znajdujących się w katalogach, a sprawdzenie ich istnienia jest niezbędne do poprawnego działania aplikacji.

# Jak to zrobić:

```Lua
if lfs.attributes("ścieżka_do_katalogu", "mode") == "directory" then
  print("Katalog istnieje!")
else
  print("Katalog nie istnieje.")
end
```

W powyższym kodzie używamy funkcji `lfs.attributes`, która zwraca informacje o podanej ścieżce. Jako drugi argument podajemy opcję "mode", która określa rodzaj poszukiwanej wartości. W przypadku sprawdzania istnienia katalogu, wybieramy opcję "directory", która zwróci wartość "true" jeśli katalog istnieje lub "false" jeśli nie istnieje.

# Mocne uderzenie

Możemy także użyć wbudowanej funkcji `io.open` z opcją "r" (czytanie pliku) do sprawdzenia istnienia katalogu:

```Lua
if io.open("ścieżka_do_katalogu", "r") then
  print("Katalog istnieje!")
else
  print("Katalog nie istnieje.")
end
```

Jest to szybsza metoda, ale nie jest zalecana do używania w przypadku większej ilości plików lub katalogów.

# Zobacz też

- Dokumentacja Lua dla funkcji `lfs.attributes`: https://www.lua.org/manual/5.4/manual.html#pdf-lfs.attributes
- Więcej informacji o wbudowanej funkcji `io.open`: https://www.lua.org/manual/5.4/manual.html#6.8
- Inne sposoby na sprawdzenie istnienia katalogu: https://stackoverflow.com/questions/9064107/does-a-folder-exist-in-lua