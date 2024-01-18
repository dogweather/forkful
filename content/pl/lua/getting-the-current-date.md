---
title:                "Pobieranie aktualnej daty"
html_title:           "Lua: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Czym jest i po co? 
Pobranie aktualnej daty w programowaniu oznacza uzyskanie informacji o bieżącym momencie czasowym, w formacie daty i godziny. Jest to przydatne dla programistów, ponieważ pozwala im na śledzenie zmian w czasie lub automatyczne umieszczanie aktualnych dat w plikach lub bazach danych. 

## Jak to zrobić: 
Aby pobrać aktualną datę w Lua, należy użyć funkcji os.date(). Przykładowy kod wygląda następująco: 
```Lua
print(os.date()) 
``` 
Output: "Tue May 11 10:26:47 2021" 

Można także ustawić własny format wyjściowy, wykorzystując specjalne symbole, np.: 
```Lua
print(os.date("%d/%m/%Y")) 
``` 
Output: "11/05/2021" 

## Głębsza analiza: 
Pobranie aktualnej daty jest niezbędną częścią programowania od lat 70. XX wieku, kiedy to powstały pierwsze języki programowania. Alternatywnym sposobem uzyskania czasu jest użycie biblioteki time, która oferuje bardziej zaawansowane funkcje. Implementacja zależy od systemu operacyjnego, na którym uruchamiamy kod, więc warto dokładnie przeanalizować działanie funkcji os.date() na różnych platformach. 

## Zobacz także: 
Więcej informacji na temat pobierania daty w Lua można znaleźć w oficjalnej dokumentacji języka: https://www.lua.org/manual/5.4/manual.html#6.9
Więcej o bibliotece time: https://www.lua.org/manual/5.4/manual.html#6.8
Alternatywnym sposobem pobierania daty jest użycie biblioteki datetime: https://github.com/luarocks/lua-datetime