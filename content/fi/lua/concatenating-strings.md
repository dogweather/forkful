---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Yhdistettyjen merkkijonojen muodostaminen - tai merkkijonokatenointi - tarkoittaa sen, että otetaan kaksi tai useampia erillisiä merkkijonoja ja yhdistetään ne yhdeksi uudeksi merkkijonoksi. Ohjelmoijat tekevät tämän usein, koska he tarvitsevat tapaa muodostaa uusia merkkijonoja olemassa olevista merkkijonoista.

# Näin se tehdään:

Merkkijonojen yhdistäminen on hyvin yksinkertaista Lua-ohjelmointikielessä. Käytämme kahta-pistettä (`..`) yhdistämään merkkijonot. Tässä on esimerkkikoodi:

```Lua
greeting = "Hei"
name = "Matti"
full_greeting = greeting .. ", " .. name
print(full_greeting)
```

Ohjelmamme tulostuksen pitäisi näyttää seuraavalta:

```Lua
Hei, Matti
```

# Syvemmälle:

Lua-ohjelmointikielen merkkijonojen yhdistäminen on peräisin sen perusmuodosta, joka on suunniteltu olemaan sekä yksinkertainen että tehokas. Historiallisesti, jotkin ohjelmointikielet, kuten C, vaativat monimutkaisempia tekniikoita merkkijonojen yhdistämiseen - kaoottisia tilanhallintakäytäntöjä ja puskureiden käsittelyä. 

Vaihtoehtoisesti, voit käyttää `string.format`-metodia, jotta voit hallita paremmin, miten merkkijonot yhdistyvät:

```Lua
name = "Matti"
greeting = string.format("Hei, %s", name)
print(greeting)
```

Tämä tuottaisi saman tulostuksen `Hei, Matti`.

Tärkeää on huomata, että `..` operaattori toimii luomalla uuden merkkijonon, joka käsittää molemmat lähtömerkkijonot. Tämä prosessi voi olla tehoton, jos yhdistät suuria merkkijonoja suurina määrinä, koska se kuluttaa ylimääräistä muistia.

# Katso myös:

Voit tutustua seuraaviin linkkeihin, jos haluat lisätietoja merkkijonojen käsittelystä Lua-ohjelmointikielessä:

1. Lua-oppikirja: Merkkijonojen käyttö: https://www.tutorialspoint.com/lua/lua_strings.htm
2. Lua : Merkkijonojen muotoilu: https://www.gammon.com.au/scripts/doc.php?general=lua_string_format