---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Lua: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Command-line argumenttien lukeminen tarkoittaa tietyn komennon antamista ohjelmalle sen käynnistämisen yhteydessä. Tämä on tärkeä osa ohjelmointia, sillä se mahdollistaa ohjelman käyttämisen eri tilanteisiin ja tarjoaa monipuolisuutta sen käyttöön.

## Miten?

Käytössäsi on useita vaihtoehtoja, joilla voit lukea command-line argumentteja Lua-kielellä. Yksi tapa on käyttää standardia `arg`-taulukkoa, joka sisältää kaikki annetut argumentit. Esimerkiksi seuraavassa koodinpätkässä tulostetaan ensimmäinen annettu argumentti:

```Lua
print(arg[1])
```

Voit myös käyttää `arg`-taulukon `select`-funktiota, joka mahdollistaa halutun argumentin valitsemisen. Esimerkiksi seuraavassa koodissa tulostetaan kolmas argumentti:

```Lua
print(select(3, table.unpack(arg)))
```

## Syvemmälle

Valmiit vaihtoehdot command-line argumenttien lukemiseen eivät aina välttämättä tarjoa riittäviä mahdollisuuksia ohjelman muokkaamiseen. Tässä tapauksessa voit käyttää `io.stdin`-tiedostoa, johon voit ohjata käyttäjän antamat argumentit.

On myös olemassa useita kirjastoja, kuten `pl` ja `cmdline`, jotka tarjoavat lisätoiminnallisuuksia command-line argumenttien käsittelyyn.

Juuret command-line argumenttien lukemisessa ovat UNIX-järjestelmissä, joissa käyttäjät tarvitsevat tapoja antaa ohjelmille tietoja niiden suorittamista varten.

## Katso myös

• Oficial Lua: https://www.lua.org/

• Lua arg documentation: https://www.lua.org/manual/5.3/manual.html#lua_env_args

• Lua stdio documentation: https://www.lua.org/pil/24.1.html