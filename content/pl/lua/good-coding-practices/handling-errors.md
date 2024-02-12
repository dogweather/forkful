---
title:                "Obsługa błędów"
aliases:
- /pl/lua/handling-errors.md
date:                  2024-01-26T00:55:36.277720-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obsługa błędów w programowaniu polega na oczekiwaniu nieoczekiwanego. To sztuka planowania na wypadek gdyby coś poszło nie tak, abyś mógł utrzymać swoją aplikację w stabilnej pracy.

## Jak to zrobić:
Lua używa dwóch głównych funkcji do obsługi błędów: `pcall` oraz `xpcall`. Oto jak ich używać:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Ups! Coś poszło nie tak.")
    else
        print("Wszystko w porządku!")
    end
end

-- Używając pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Sukces!")
else
    print("Złapany błąd:", errorMessage)
end

-- Używając xpcall z obsługą błędów
function myErrorHandler(err)
    print("Zarządca błędów mówi:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Czy wywołanie było udane?", status)
```

Przykładowy wynik może być:

```
Złapany błąd: Ups! Coś poszło nie tak.
Zarządca błędów mówi: Ups! Coś poszło nie tak.
Czy wywołanie było udane? false
```
Lub jeśli nie nastąpi błąd:
```
Wszystko w porządku!
Sukces!
Wszystko w porządku!
Czy wywołanie było udane? true
```

## Szczegółowa analiza
Obsługa błędów, czyli "obsługa wyjątków", nie zawsze była rzeczą oczywistą. Wczesne programy często się zawieszały. Wraz z rozwojem kodowania pojawiła się również potrzeba stabilności. Podejście Luay jest proste w porównaniu z innymi językami. Nie ma bloków `try/catch`, są tylko `pcall` i `xpcall`. Pierwsza zabezpiecza wywołanie funkcji, zwracając status i jakiś błąd. Druga dodaje funkcję obsługi błędów, co jest przydatne do własnej obsługi błędów lub logowania.

Alternatywą w Lua jest użycie `assert`, które może służyć podobnemu celowi, rzucając błąd, jeśli jego warunek jest fałszywy. Jednak nie jest tak elastyczne jak `pcall` do skomplikowanych scenariuszy obsługi błędów.

Wewnętrznie, `pcall` oraz `xpcall` działają poprzez ustawienie "ochronionego środowiska" dla funkcji do uruchomienia. Jeśli pojawi się błąd, środowisko łapie go i może albo zaraz go obsłużyć, albo przekazać go z powrotem, aby program mógł sobie z nim poradzić.

## Zobacz również
- Książka "Programming in Lua" (trzecie wydanie), dostępna na https://www.lua.org/pil/ do dokładnego przeczytania na temat obsługi błędów (Sekcja 8.4).
- Oficjalny podręcznik Lua 5.4, dostępny na stronie: https://www.lua.org/manual/5.4/ - dla najbardziej aktualnych informacji o funkcjach obsługi błędów w Lua.
- Wiki użytkowników Lua na temat obsługi błędów: http://lua-users.org/wiki/ErrorHandling – dla społecznościowych wglądów i wzorców.
