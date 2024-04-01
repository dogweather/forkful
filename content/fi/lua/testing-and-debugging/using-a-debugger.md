---
date: 2024-01-26 03:50:47.916716-07:00
description: "Debugger on ty\xF6kalu, jonka avulla voit tarkastella ja ohjata ohjelman\
  \ suoritusta, mik\xE4 tekee virheiden paikallistamisesta helppoa. Ohjelmoijat k\xE4\
  ytt\xE4v\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.701626-06:00'
model: gpt-4-0125-preview
summary: "Debugger on ty\xF6kalu, jonka avulla voit tarkastella ja ohjata ohjelman\
  \ suoritusta, mik\xE4 tekee virheiden paikallistamisesta helppoa. Ohjelmoijat k\xE4\
  ytt\xE4v\xE4t\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

## Kuinka:
Lua ei sisällä sisäänrakennettua debuggeria, mutta voit käyttää ulkoisia, kuten ZeroBrane Studio. Tässä maistiainen siitä, miten sen kanssa työskentelisit:

```Lua
-- Tämä on yksinkertainen Lua-skripti tahallisella virheellä
local function add(a, b)
    local result = a + b -- Oho, teeskennellään, että unohdimme määritellä 'b':n
    return result
end

print(add(10))
```

Kun suoritat tämän debuggerissa, se pysäyttää suorituksen, kun asiat menevät pieleen. Näet jotain tällaista:

```
lua: example.lua:3: yritys suorittaa aritmeettista operaatiota arvolla nil (paikallinen 'b')
pinon jäljitelmä:
	example.lua:3: funktiossa 'add'
	example.lua:7: päälohkossa
	[C]: in ?
```

Voit asettaa keskeytyspisteitä, kulkea koodisi läpi askel askeleelta ja kurkistaa muuttujien arvoihin jäljittääksesi vian menettämättä järkeäsi.

## Syväsukellus
Valitettavasti Luassa yksinkertaisuus ei ulotu debuggaukseen. Ei hätää kuitenkaan, Lua-yhteisö pitää sinusta huolen. Työkalut kuten ZeroBrane Studio, LuaDec ja muut tarjoavat debuggauskykyjä. Historiallisesti debuggerit ovat olleet olemassa pian ensimmäisten ohjelmien happamien käänteiden jälkeen, antaen kehittäjille keinot korjata koodiaan sohimatta sokkona.

Luassa usein nojaudutaan ulkoisiin debuggereihin tai rakennetaan ne kehitysympäristöösi. Esimerkiksi ZeroBrane Studio on IDE, joka integroi täysin Luas-debuggerin. Se antaa sinun kulkea koodin läpi askel askeleelta, asettaa keskeytyspisteitä ja tarkkailla muuttujia. Toteutuspuolella debuggerit käyttävät tavallisesti koukkuja keskeytyspisteiden ja muiden debuggausvälineiden lisäämiseen.

Vaihtoehtoja? Ehdottomasti. Hyvät vanhat `print`-lauseet, hellästi kutsuttu "printf-debuggaus", voivat joskus tehdä tehtävän ilman hienoja työkaluja.

## Katso myös
Jatkaaksesi debuggausmatkaasi, tarkista:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-käyttäjien wiki Debugging Lua Code -osiossa: http://lua-users.org/wiki/DebuggingLuaCode
- `debug`-kirjaston viite Luas käsikirjassa: https://www.lua.org/manual/5.4/manual.html#6.10
