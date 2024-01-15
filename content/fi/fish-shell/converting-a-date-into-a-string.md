---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Fish Shell: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi Fish Shell -ohjelmoinnissa? On olemassa monta syytä, kuten halu tallentaa päivämäärät tietokantaan, muokata niitä tai käyttää niitä osana tiedoston nimeä.

## Miten tehdä se

Fish Shell tarjoaa useita tapoja muuttaa päivämäärä merkkijonoksi. Voit käyttää "date" -komentoa, joka tulostaa nykyisen päivämäärän ja ajan standardimuodossa.

```
fish_shell> date
pe 14 toukokuu 2021 15.49.18 EEST
```

Voit myös muuttaa päivämäärän halutun muodon mukaiseksi käyttämällä "-f" -valitsinta ja määrittämällä muodon merkkijonossa.

```
fish_shell> date -f %d-%m-%Y
14-05-2021
```

## Syvällisempi sukellus

Fish Shellin date-komento käyttää "GNU date" -ohjelmaa, joka tarjoaa monia eri muotoiluvaihtoehtoja. Voit löytää lisätietoja GNU daten käytöstä komennolla "man date" tai käyttämällä "date --help".

Voit myös muuttaa päivämäärän muotoa ottamalla ensin talteen haluamasi päivämäärän käyttämällä "set" -komentoa, ja sitten muuttamalla sen merkkijonoksi käyttäen "string" -komentoa.

```
fish_shell> set day (date +%B-%d-%Y)
fish_shell> string $day
toukokuu-14-2021
```

## Katso myös

- [Fish Shellin dokumentaatio "Date"-komento](https://fishshell.com/docs/current/cmds/date.html)
- [GNU daten virallinen dokumentaatio](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)