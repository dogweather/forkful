---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces, w którym program komputerowy odbiera i interpretuje dane z pliku zapisanego w formacie tekstowym. Programiści robią to, aby uzyskać dane do przetwarzania lub modyfikacji.

## Jak to zrobić:

Załóżmy, że mamy plik o nazwie 'tekst.txt'. Aby odczytać ten plik w Lua, użyjemy następujących linii kodu:

```Lua
local file = io.open("tekst.txt", "r")  -- otwórz plik do odczytu

if file then  
    for line in file:lines() do  -- iteruj poprzez każdą linię
        print(line)  -- wyświetl zawartość linii
    end
    file:close()  -- zawsze pamiętaj, aby zamknąć plik
else
    print("Nie udało się otworzyć pliku.")
end
```

Jeżeli plik "tekst.txt" zawiera następujące linie:
```
Witaj Świecie!
Jak się masz?
```

To wyjście programu będzie takie samo:
```
Witaj Świecie!
Jak się masz?
```


## Więcej szczegółów:
1) **Kontekst historyczny:** Lua, będący językiem skryptowym, zawsze zapewniał prosty sposób do interakcji z zasobami systemowymi, takimi jak pliki tekstowe.
   
2) **Alternatywy:** Można także odczytać cały plik tekstowy naraz za pomocą funkcji `read("*a")`, a następnie zinterpretować dane według potrzeb. Choć jest to wygodne, lepiej unikać tego przy dużej ilości danych, ponieważ może to prowadzić do problemów z wydajnością.
   
3) **Szczegóły implementacji:** W powyższym przykładzie użyliśmy biblioteki I/O Lua. `io.open` służy do otwarcia pliku, `file:lines()` do iterowania przez linie pliku, a `file:close()` do zamknięcia pliku po zakończeniu.

## Zobacz również:
- Dokumentacja Lua na temat I/O: https://www.lua.org/pil/21.1.html
- Przewodnik Lua I/O: https://www.tutorialspoint.com/lua/lua_file_io.htm