---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen interpolaatio on tapa lisätä muuttujien arvoja suoraan merkkijonojen sisälle. Tätä tekniikkaa käyttävät ohjelmoijat tuottaakseen dynaamisen ja lueteltavissa olevan tekstin.

## Miten tehdä:
Käytetään esimerkkinä `string.format`-funktioon. 

```Lua
nimi = "Erkki"
tervehdys = string.format("Hei, %s", nimi)
print(tervehdys)
```

Ohjelman tulostus:
```Lua
Hei, Erkki
```
`%s` toimii paikkamerkkinä `string.format` -funktiossa, joka täytetään muuttujan `nimi` arvolla.

## Syvällisemmin:
Valitettavasti Lua ei sisällä sisäänrakennettua merkkijonojen interpolaatiota, kuten jotkut muut ohjelmointikielet (esimerkiksi Python tai JavaScript). Historiallisesti merkkijonojen interpolaatio ei ollut suosittu ominaisuus, mutta se on lisätty useimpiin ohjelmointikieliin sen tuottaman koodin selkeyden ja helppokäyttöisyyden vuoksi.

Vaihtoehtoina merkkijonojen interpolaatioon on kaksi yleistä metodia: konkatenointi ja formatointi. Olemme jo nähneet `string.format`-funktion aiemmin.

Merkkijonojen suiskeiden toteutus yksityiskohdat vaihtelevat ohjelmointikielen mukaan. Joissakin, kuten Python, on sisäänrakennettu tuki, toisissa, kuten JavaScript, on käytössä erityinen merkkijonoliteraali (``). Luassa se on toteutettu `string.format`-funktion avulla.

## Ks myös:
- Lua: Kirjaston merkkijonotoiminnat: https://www.lua.org/manual/5.4/manual.html#6.4
- Stack Overflow: Lua merkkijonojen interpolaatio: https://stackoverflow.com/questions/9145432/lua-string-interpolation