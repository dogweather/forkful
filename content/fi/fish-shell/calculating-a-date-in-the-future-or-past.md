---
title:                "Menneen tai tulevan päivämäärän laskeminen"
html_title:           "Fish Shell: Menneen tai tulevan päivämäärän laskeminen"
simple_title:         "Menneen tai tulevan päivämäärän laskeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa saatat tarvita laskemaan tulevan tai menneen päivämäärän. Ehkä haluat suunnitella tulevia tapahtumia tai tarkistaa tietyn päivän sääennusteen. Fish Shellin avulla tämä on helppoa ja nopeaa tehdä.

## Miten

Voit käyttää `date`-komentoa laskemaan päivämäärän tulevaisuudessa tai menneisyydessä. Esimerkiksi, jos haluat tietää millainen päivämäärä on kahden viikon päästä, kirjoita komentoriville:

```
Fish Shell date -v +2w
```

Tämä tulostaa päivämäärän kahden viikon päässä nykyhetkestä. Voit myös käyttää `+` tai `-` merkkejä ilmaisemaan tulevaisuuden tai menneisyyden. Esimerkiksi `+2d` tarkoittaa kahden päivän päästä ja `-1m` tarkoittaa yhtä kuukautta sitten.

Voit myös muuttaa formaattia lisäämällä `+`-merkin perään haluamasi formaatin. Esimerkiksi, `+2w %d-%m-%y` tulostaa päivämäärän kahden viikon päästä muodossa "päivä-kuukausi-vuosi".

## Syvempi sukellus

Fish Shellin `date`-komento perustuu POSIX-standardiin, joka määrittää päivämäärän laskemisen tietyn kaavan mukaan. Voit lukea lisää tästä kaavasta ja sen eri vaihtoehdoista `date`-komenton manuaalisivulta (`man date`).

Jos haluat lisätietoa päivämäärän laskemisesta, voit myös tutustua `date`-komenton lähdekoodiin Fish Shellin GitHub-sivustolla.

## Katso myös

- [Fish Shellin kotisivu](https://fishshell.com/)
- [Fish Shellin manuaalisivu](https://fishshell.com/docs/current/index.html)
- [Fish Shellin GitHub-sivu](https://github.com/fish-shell/fish-shell)