---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Tulostus virheenjäljityksen: mitä, miksi, ja miten? Lua ohjelmoinnissa

## Mitä ja Miksi?
Virheenjäljityksen tulostus on keino nähdä ohjelman toiminta sen suorituksen aikana. Se on tärkeä, koska se tarjoaa yksityiskohtaisen kuvan siitä, mitä sovelluksesi tekee juuri nyt.

## Kuinka?
Käytämme `print()` funktiota tulostamaan virheenjäljityksen tulostuksen. Katso alla olevat esimerkit:

```Lua
-- Luodaan muuttuja
local muuttuja = "Tämä on testi!"
-- Tulostetaan muuttuja
print(muuttuja)
```
Lähtö:
```
Tämä on testi!
```
## Deep Dive
Historiallisesti, tulostus on aina ollut olennainen osa virheenjäljitystä. Se on nopea ja tehokas tapa nähdä, mitä ohjelmasi tekee reaaliajassa.
Vaihtoehtoinen lähestymistapa olisi käyttää debuggaaja, joka antaa enemmän hallintaa ja tietoa, mutta se voi olla ylimitoitettu yksinkertaisiin tarpeisiin.

`print()` on yksinkertainen ja tehokas työkalu. Mutta on olemassa kehittyneempiä työkaluja kuten `io.write()` jos haluat suuremman määrän kontrolia.

## Katso myös
Tutustu näihin linkkeihin saadaksesi lisätietoja:
- Lua virallinen dokumentaatio: https://www.lua.org/docs.html
- Debuggaus Lua -kirjastoissa: https://www.lua.org/pil/23.1.html
- Kehittyneitä virheenjäljitystekniikoita: https://www.lua.org/pil/23.2.html