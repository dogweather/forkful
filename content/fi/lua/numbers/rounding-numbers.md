---
aliases:
- /fi/lua/rounding-numbers/
date: 2024-01-26 03:46:13.983645-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai m\xE4\xE4riteltyyn desimaalipaikkaan. Se\
  \ on ohjelmoinnissa peruskauraa\u2026"
lastmod: 2024-02-18 23:09:07.751924
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai m\xE4\xE4riteltyyn desimaalipaikkaan. Se\
  \ on ohjelmoinnissa peruskauraa\u2026"
title: "Numerojen py\xF6rist\xE4minen"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Numeroiden pyöristäminen tarkoittaa niiden säätämistä lähimpään kokonaislukuun tai määriteltyyn desimaalipaikkaan. Se on ohjelmoinnissa peruskauraa monimutkaisuuden vähentämiseksi, suorituskyvyn parantamiseksi ja silloin, kun tarkkuus tietyn pisteen jälkeen ei lisää arvoa.

## Miten:
```lua
-- Perus pyöristäminen Luassa ei tule valmiina, mutta voit määritellä funktion:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Pyöristääksesi tiettyyn desimaalipaikkaan:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Syväsukellus
Lua ei sisällä valmiina pyöristysfunktiota toisin kuin jotkin muut kielet. Historiallisesti sinun on täytynyt kirjoittaa oma tai käyttää kolmannen osapuolen kirjastoa. Yleisiä kiertotapoja ovat `math.floor()` käyttäminen pyöristettäessä alas ja `math.ceil()` pyöristettäessä ylös, yhdistettynä 0.5 lisäämiseen tai vähentämiseen ennen sitä riippuen numeron merkistä.

Vaihtoehtoja oman funktion kirjoittamiselle ovat kirjastot, kuten "lua-users wiki" tai "Penlight". Kullakin on etunsa ja haittansa, kuten lisäominaisuudet tai suurempi ylikuormitus.

Sisäisesti nämä funktiot toimivat yleensä hyödyntämällä sitä, miten tietokoneet tallentavat liukulukuarvoja. Positiivisen liukuluvun, jonka haluat pyöristää, lisääminen 0.5:llä työntää sen seuraavan kokonaislukualueen kynnyksen yli, joten kun käytät `math.floor()`-funktiota, se pyöristää sen lähimpään kokonaislukuun.

## Katso myös
- [Lua 5.4 Referenssiopas: Matemaattiset funktiot](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Kirjastot: Matematiikka](https://github.com/lunarmodules/Penlight)
