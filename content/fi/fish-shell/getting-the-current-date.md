---
title:                "Fish Shell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat tietää nykyisen päivämäärän Fish Shellissä. Ehkä haluat käyttää sitä osana skriptiä, joka suoritetaan aina tietyllä päivämäärällä. Tai ehkä tarvitset sitä osaksi laskentaa tai muuta tietokoneellista työtä. Joka tapauksessa, Fish Shell tarjoaa helpon ja nopean tavan saada nykyinen päivämäärä ohjelmallisesti.

## Kuinka tehdä

Fish Shellin `date` komennolla voit saada nykyisen päivämäärän eri muodoissa käyttämällä erilaisia lipukkeita (flags). Esimerkiksi `-u` flagi näyttää päivämäärän UTC:a vastaavassa aikavyöhykkeessä ja `-R` flagi näyttää päivämäärän RFC 2822 formaatissa. Voit myös muokata päivämäärän ulkoasua käyttämällä `date` komennolle annettua tiettyä formaattia. Alla on esimerkki siitä, kuinka saat nykyisen päivämäärän formaatissa "kuukausi/päivä/vuosi":

```
Fish Shell: date +%m/%d/%Y
01/15/2020
```

## Syvemmälle

Fish Shellin `date` komento käyttää käyttöjärjestelmän `date` komentoa taustalla. Tämä tarkoittaa sitä, että voit käyttää kaikkia `date` komennon paikallisia flagimahdollisuuksia Fish Shellissä. Voit esimerkiksi käyttää `-j` flagia saadaksesi päivämäärän suoraan sekunteina:

```
Fish Shell: date -j
1547606840
```

Voit myös käyttää `date` komentoa muissa skripteissä tai ohjelmissa, mikäli haluat sisällyttää nykyisen päivämäärän tietoon. Esimerkiksi, voit tallentaa nykyisen päivämäärän muuttujaan ja käyttää sitä ohjelmallisesti myöhemmin.

## Katso myös

- Fish Shellin viralliset dokumentaatiot (englanniksi): https://fishshell.com/docs/current/cmds/date.html
- `date` komennon man-sivu (englanniksi): https://linux.die.net/man/1/date
- Fish Shellin GitHub-sivu (englanniksi): https://github.com/fish-shell/fish-shell