---
title:                "Zaokrąglanie liczb"
aliases:
- /pl/lua/rounding-numbers.md
date:                  2024-01-26T03:45:54.975555-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zaokrąglanie liczb oznacza dostosowywanie ich do najbliższej liczby całkowitej lub określonego miejsca po przecinku. Jest to podstawowy element w programowaniu służący redukcji złożoności, zwiększaniu wydajności oraz na potrzeby sytuacji, gdy precyzja poza pewnym punktem nie dodaje wartości.

## Jak to zrobić:
```lua
-- Podstawowe zaokrąglanie w Lua nie jest dostępne od razu, ale można zdefiniować funkcję:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Aby zaokrąglić do określonego miejsca po przecinku:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Szczegółowa analiza
Lua standardowo nie zawiera funkcji zaokrąglania, w przeciwieństwie do niektórych innych języków. Historycznie, musisz napisać własną funkcję lub użyć biblioteki firm trzecich. Powszechne obejścia polegają na stosowaniu `math.floor()` do zaokrąglania w dół i `math.ceil()`, do zaokrąglania w górę, połączone z dodawaniem lub odejmowaniem 0,5 przed dokonaniem tego, w zależności od znaku liczby.

Alternatywy dla tworzenia własnej funkcji obejmują biblioteki takie jak "lua-users wiki" czy "Penlight". Każda z nich ma swoje zalety i wady, takie jak dodatkowe funkcje lub większe obciążenie.

Wewnętrznie, te funkcje zazwyczaj działają, wykorzystując sposób, w jaki komputery przechowują liczby zmiennoprzecinkowe. Dodanie 0.5 do dodatniej liczby zmiennoprzecinkowej, którą chcesz zaokrąglić, przesunie ją ponad próg następnej wartości całkowitej, więc gdy zastosujesz `math.floor()`, odbywa się zaokrąglenie do najbliższej liczby całkowitej.

## Zobacz też
- [Lua 5.4 Referencyjny Podręcznik: Funkcje matematyczne](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Biblioteki Lua Penlight: Matematyka](https://github.com/lunarmodules/Penlight)
