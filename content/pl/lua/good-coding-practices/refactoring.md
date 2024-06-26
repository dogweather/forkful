---
date: 2024-01-26 01:47:03.192289-07:00
description: "Jak to zrobi\u0107: We\u017Amy prost\u0105 funkcj\u0119 Lua i dokonajmy\
  \ jej refaktoryzacji. Zaczynamy od funkcji, kt\xF3ra oblicza sum\u0119 liczb w li\u015B\
  cie, ale zosta\u0142a napisana bez\u2026"
lastmod: '2024-03-13T22:44:35.548405-06:00'
model: gpt-4-0125-preview
summary: "We\u017Amy prost\u0105 funkcj\u0119 Lua i dokonajmy jej refaktoryzacji."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Weźmy prostą funkcję Lua i dokonajmy jej refaktoryzacji. Zaczynamy od funkcji, która oblicza sumę liczb w liście, ale została napisana bez większego zastanowienia nad wydajnością czy jasnością:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Wyświetla: 10
```

Refaktoryzacja do wersji bardziej wydajnej i czytelnej:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Nadal wyświetla: 10
```

Zrefaktoryzowana wersja pozbywa się zbędnego wewnętrznego pętli, używając `ipairs` do czystego iterowania przez listę.

## Wnikliwe spojrzenie
Historycznie rzecz biorąc, refaktoryzacja wywodzi się ze społeczności programistycznej Smalltalk na koniec lat 80. i została spopularyzowana przez książkę Martina Fowlera 'Refaktoryzacja: Ulepszanie struktury istniejącego kodu'. W Lua, refaktoryzacja często obejmuje upraszczanie złożonych warunków, dzielenie dużych funkcji na mniejsze, oraz optymalizację użycia tabel w celu poprawy wydajności.

Refaktoryzacja w Lua ma swoje pułapki; dynamiczna natura Lua i elastyczne typowanie mogą czynić pewne refaktoryzacje, jak zmiana nazw zmiennych czy sygnatur funkcji, ryzykownymi, jeśli nie są wykonywane ostrożnie. Narzędzia do statycznej analizy kodu (jak `luacheck`) mogą zmniejszyć takie ryzyko. Alternatywy obejmują rozwój oparty na testach (TDD), gdzie kod jest ciągle refaktoryzowany jako integralna część procesu deweloperskiego, w przeciwieństwie do osobnej fazy refaktoryzacji.

## Zobacz również
- "Programowanie w Lua" autorstwa Roberta Ierusalimschy'ego dla najlepszych praktyk i przykładów.
- "Refaktoryzacja: Ulepszanie struktury istniejącego kodu" autorstwa Martina Fowlera dla zasad stosowanych w różnych językach.
- Katalog LuaRocks (https://luarocks.org/) dla narzędzi i modułów skierowanych na utrzymanie i refaktoryzację kodu Lua.
