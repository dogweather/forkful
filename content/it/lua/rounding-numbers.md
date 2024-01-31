---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:45:57.439841-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Arrotondare i numeri significa aggiustarli all'intero più vicino o a un punto decimale specificato. È un elemento fondamentale nella programmazione per ridurre la complessità, migliorare le prestazioni e per i momenti in cui la precisione oltre un certo punto non aggiunge valore.

## Come fare:
```lua
-- L'arrotondamento base in Lua non è incorporato, ma puoi definire una funzione:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Per arrotondare a un punto decimale specifico:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Approfondimento
Lua non include una funzione di arrotondamento pronta all'uso a differenza di alcuni altri linguaggi. Storicamente, è necessario scrivere la propria funzione o utilizzare una libreria di terze parti. Le soluzioni comuni si affidano a `math.floor()` per l'arrotondamento verso il basso e `math.ceil()` per l'arrotondamento verso l'alto, abbinati all'aggiunta o alla sottrazione di 0.5 prima di farlo, a seconda del segno del numero.

Le alternative all'elaborazione della propria funzione includono librerie come "lua-users wiki" o "Penlight". Ognuna ha i suoi vantaggi e svantaggi, come funzionalità aggiuntive o maggiore overhead.

Internamente, queste funzioni lavorano normalmente sfruttando il modo in cui i computer memorizzano i numeri in virgola mobile. Aggiungere 0.5 a un float positivo che si desidera arrotondare lo spingerà oltre la soglia del prossimo valore intero, quindi quando si applica `math.floor()` viene arrotondato verso il basso a quell'intero più vicino.

## Vedere Anche
- [Manuale di Riferimento Lua 5.4: Le Funzioni Matematiche](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Librerie Lua Penlight: Math](https://github.com/lunarmodules/Penlight)
