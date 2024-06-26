---
date: 2024-01-26 04:43:35.455821-07:00
description: "Jak to zrobi\u0107: W Lua mo\u017Cna reprezentowa\u0107 liczby zespolone\
  \ za pomoc\u0105 tabel. Podstawowe operacje obejmuj\u0105 dodawanie, odejmowanie,\
  \ mno\u017Cenie i dzielenie tych\u2026"
lastmod: '2024-03-13T22:44:35.534019-06:00'
model: gpt-4-0125-preview
summary: "W Lua mo\u017Cna reprezentowa\u0107 liczby zespolone za pomoc\u0105 tabel."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
W Lua można reprezentować liczby zespolone za pomocą tabel. Podstawowe operacje obejmują dodawanie, odejmowanie, mnożenie i dzielenie tych tabel. Oto jak:

```lua
-- Zdefiniowanie dwóch liczb zespolonych jako tabele
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Funkcja do dodawania dwóch liczb zespolonych
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Przykładowe wyjście
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Szczegółowa analiza
Liczby zespolone istnieją od XVI wieku, pomagając rozwiązywać równania, z którymi nie można było sobie poradzić używając tylko liczb rzeczywistych. Lua sama w sobie nie ma wbudowanego typu liczby zespolonej. Jednakże to nie problem - możesz stworzyć własne manipulacje liczbami zespolonymi, używając tabel i funkcji, jak pokazano powyżej. Lub, jeśli Twoje potrzeby są bardziej zaawansowane, wybierz bibliotekę jak LuaComplex. To dobry wybór, ponieważ jest skonstruowana specjalnie dla Lua i zabiera z rąk ręczną pracę. Biblioteki tego typu często również optymalizują operacje w tle, więc są szybsze niż tworzenie własnych rozwiązań.

## Zobacz także
Po więcej szczegółowych przykładów i zaawansowanych operacji sprawdź:

- Biblioteka LuaComplex: https://github.com/davidm/lua-complex
- Książka "Programming in Lua", do tworzenia własnych typów danych: https://www.lua.org/pil/11.1.html
- Wikipedia o zastosowaniach liczb zespolonych w różnych dziedzinach: https://en.wikipedia.org/wiki/Complex_number#Applications
