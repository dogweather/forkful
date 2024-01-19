---
title:                "Merkkijonon pääkirjaimet suuriksi"
html_title:           "Lua: Merkkijonon pääkirjaimet suuriksi"
simple_title:         "Merkkijonon pääkirjaimet suuriksi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Merkkijonon isojen alkukirjainten tekeminen tarkoittaa jokaisen sanan ensimmäisen kirjaimen muuttamista kirjoitusasuun, jossa se on suuri. Ohjelmoijat käyttävät tätä tehokkaasti parantaakseen käyttöliittymää ja tekstin luettavuutta.

## Kuinka Tehdään:
Tässä on esimerkki siitä, kuinka suuriksi muunnos voidaan tehdä Lua-ohjelmointikielellä:
```Lua
function string.cap(s)
   return s:lower():gsub("^%l", string.upper)
end
print(string.cap("hello world"))
```
Tämä esimerkkikoodi antaa tuloksen:
```Lua
Hello world
```
## Syvä Sukellus:
Alkuperäisessä Lua versiossa ei ollut sisäänrakennettua toimintoa tämäntyyppiseen muunnokseen. Kuitenkin nykyaikaisissa versioissa se voidaan tehdä gsub-menetelmää ja muita merkkijonojen käsittelymetodeja käyttämällä, kuten esimerkiksi yllä olevassa koodinpätkässä.

Vaihtoehtoja ovat sen tekeminen manuaalisesti loop-iteraatioilla ja tilapäisten muuttujien avulla. Jotkut kehittäjät saattavat käyttää myös kolmannen osapuolen kirjastoja tälle toimenpiteelle.

Tällaisen toiminnon toteuttamisen monimutkaisuus riippuu siitä, kuinka kattavasti kehittäjä haluaa ottaa huomioon erilaiset edge-case-tilanteet, kuten erikoismerkit, numerot tai ääkköset.

## Katso Myös:
- Lua string library: [String Manipulation](https://www.lua.org/pil/20.html)
- Advanced string manipulation: [Lua-Users wiki strings tutorial](http://lua-users.org/wiki/StringsTutorial) 
- Usage of upper function: [Lua string.upper](https://www.lua.org/manual/5.4/manual.html#6.4)