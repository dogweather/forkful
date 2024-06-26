---
date: 2024-01-26 03:45:54.975555-07:00
description: "Jak to zrobi\u0107: Lua standardowo nie zawiera funkcji zaokr\u0105\
  glania, w przeciwie\u0144stwie do niekt\xF3rych innych j\u0119zyk\xF3w. Historycznie,\
  \ musisz napisa\u0107 w\u0142asn\u0105\u2026"
lastmod: '2024-04-05T21:53:36.963753-06:00'
model: gpt-4-0125-preview
summary: "Lua standardowo nie zawiera funkcji zaokr\u0105glania, w przeciwie\u0144\
  stwie do niekt\xF3rych innych j\u0119zyk\xF3w."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

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
