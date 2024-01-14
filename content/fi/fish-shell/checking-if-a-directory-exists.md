---
title:                "Fish Shell: Tarkista onko hakemistoa olemassa"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemiston olemassaolo?

Monissa ohjelmointiprojekteissa on tarpeen tarkistaa, onko tietty hakemisto olemassa ennen kuin siihen tehdään muutoksia. Tämä voi olla hyödyllistä esimerkiksi skriptejä ja komentorivikomentoja käytettäessä. Tässä blogikirjoituksessa opit, miten voit tarkistaa hakemiston olemassaolon Fish Shell käyttämällä.

## Miten tehdä se?

Ensinnäkin, voit käyttää komentoa `test` tarkistaaksesi, onko hakemisto olemassa. Tämä komento palauttaa arvon 0, jos hakemisto on olemassa, ja arvon 1, jos sitä ei ole.

```Fish Shell
if test -d "hakemiston_nimi"
    echo "Hakemisto on olemassa."
else
    echo "Hakemisto ei ole olemassa."
end
```

Tämä koodi tarkistaa, onko `hakemiston_nimi` niminen hakemisto olemassa ja tulostaa sen mukaisen viestin.

Voit myös käyttää komentoa `stat` tarkistaaksesi hakemiston tilasta. Tämä komento palauttaa tietoja tiedostosta tai hakemistosta, ja sen avulla voit tarkistaa, onko hakemisto olemassa.

```Fish Shell
if stat -t "hakemiston_nimi" > /dev/null 2>&1
    echo "Hakemisto on olemassa."
else
    echo "Hakemisto ei ole olemassa."
end
```

Huomaa, että tässä käytetään hyödyksi `> /dev/null 2>&1` -merkintää, joka ohjaa komennon tulosteen pois näkyvistä, jottei sitä tulosteta ruudulle.

## Syvempi sukellus

Fish Shellin manuaalisivulta löytyy lisätietoja näistä ja muista hakemiston olemassaolon tarkistamiseen liittyvistä komennoista. Voit myös käyttää komentoa `help [komento]` saadaksesi tarkempia tietoja tietyistä komennoista, esimerkiksi `help test` tai `help stat`.

## Katso myös

- [Fish Shellin manuaalisivut](https://fishshell.com/docs/current/index.html)
- [Fish Shellin ohjeet ja esimerkit](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shellin Slack-yhteisö](https://fishshell.com/docs/current/tutorial.html)