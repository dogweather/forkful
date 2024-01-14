---
title:                "Fish Shell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmoinnin prosessia useimmilla kielillä, mukaan lukien Fish Shell. Se auttaa tehokkaasti muokkaamaan ja päivittämään suuria määriä tekstiä ilman manuaalista työskentelyä. Lue eteenpäin oppiaksesi, miten se tehdään Fish Shellillä.

## Miten

```Fish Shell
sed -i 's/vanha/uusi/g' tiedosto.txt
```

Tässä esimerkissä käytämme komentoa `sed` korvaamaan kaikki "vanha" sanat "uusi" sanalla tiedosto.txt-tiedostossa. Toiminnon `-i` avulla muutokset tallennetaan suoraan tiedostoon. Voit myös tehdä vaikutuksia useisiin tiedostoihin kerralla käyttämällä haku- ja korvausoperaattoreita. Lisätietoja voit lukea Fish Shellin manuaalista käyttämällä komentoa` man sed`. Voit myös käyttää muita komentoja, kuten `awk` ja `find`, tekstin etsimiseksi ja korvaamiseksi.

## Deep Dive

Voit etsiä ja korvata vain tiettyjä merkkijonoja käyttämällä säännöllisiä lausekkeita (regular expressions). Voit käyttää `sed`-komentoa tai Fish Shellin `string replace` -toimintoa. Voit myös käyttää muuttujia ja ehtolausekkeita, jotta voit etsiä ja korvata eri merkkijonoja eri tilanteissa.

## Katso myös

- [Fish Shell Manuaali](https://fishshell.com/docs/current/#overview)
- [sed-komento](https://www.gnu.org/software/sed/manual/sed.html)
- [string replace - Fish Shellin ohjeet](https://fishshell.com/docs/current/commands.html#string-replace)