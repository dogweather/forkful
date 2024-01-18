---
title:                "Kahden päivämäärän vertailu"
html_title:           "Lua: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärien vertaaminen tarkoittaa kahden eri päivämäärän arvojen vertaamista, jotta voidaan selvittää kumpi niistä on suurempi tai pienempi. Tätä tarvitaan usein ohjelmoinnissa esimerkiksi varmistaaksemme, että tietokone käsittelee päivämääriä oikeassa järjestyksessä ja näyttää oikeita tietoja käyttäjilleen.

## Miten:
Vertaamme kahta päivämäärää käyttäen `os.time` ja `os.difftime` -funktioita, jotka muuttavat päivämäärät aikaleimoiksi ja laskevat niiden välillä olevan eron sekunneissa. Käytämme myös `if`-ehtolauseita vertailun tekemiseen ja `print`-komennon avulla tulostamme eri vaihtoehdot.

```Lua
local date1 = os.time({year = 2020, month = 11, day = 15})
local date2 = os.time({year = 2020, month = 3, day = 10})

if os.difftime(date1, date2) > 0 then
    print("Päivämäärä 1 on suurempi kuin päivämäärä 2.")
elseif os.difftime(date1, date2) < 0 then
    print("Päivämäärä 2 on suurempi kuin päivämäärä 1.")
else
    print("Päivämäärät ovat samat.")
end

--> Tulostaa: Päivämäärä 1 on suurempi kuin päivämäärä 2.
```

## Syvempi sukellus:
Historiallisesti tietokoneissa päivämääriä on tallennettu sekunteina, jotka ovat kuluneet tietystä hetkestä, yleensä vuoden 1970 alusta. Tämä on nimeltään "epoch" ja se voi vaihdella eri käyttöjärjestelmissä. Myös erilaisia algoritmeja on kehitetty päivämäärien vertailuun, mutta `os.difftime`-funktio on yleisesti hyväksytty ja helppokäyttöinen ratkaisu.

## Katso myös:
- [Lua-käsikirja](https://www.lua.org/manual/5.4/)
- [Vinkkejä päivämäärien käsittelyyn Lua:ssa](https://www.computercraft.info/wiki/Os.date_(function))