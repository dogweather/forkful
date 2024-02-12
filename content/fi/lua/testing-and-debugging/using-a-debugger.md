---
title:                "Debuggerin käyttö"
aliases: - /fi/lua/using-a-debugger.md
date:                  2024-01-26T03:50:47.916716-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Debugger on työkalu, jonka avulla voit tarkastella ja ohjata ohjelman suoritusta, mikä tekee virheiden paikallistamisesta helppoa. Ohjelmoijat käyttävät debuggereita matojen murskaamiseen, koodivirran ymmärtämiseen ja varmistaakseen, että heidän koodinsa on niin puhdasta kuin pilli.

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
