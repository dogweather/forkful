---
title:                "Nykyisen päivämäärän hakeminen"
html_title:           "Bash: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi hakea nykyisen päivämäärän? Yksinkertaisesti siksi, että päivämäärä on usein tarpeellinen tieto erilaisissa skripteissä ja ohjelmissa. Esimerkiksi jos haluat luoda tiedostoihin päivämäärän perusteella nimet, tarvitset tähän tiedon.

## Näin teet sen

```Bash
date
```

Yllä oleva komento tulostaa nykyisen päivämäärän ja kellonajan seuraavassa muodossa: "ma touko 17 11:14:54 EEST 2021".

Voit myös lisätä parametreja komentoon saadaksesi päivämäärästä haluamasi tiedot. Esimerkiksi:

```Bash
date +"%d.%m.%Y"
```

Tämä tulostaa päivämäärän muodossa "17.05.2021". Voit löytää lisää mahdollisia parametreja Bashin manuaalisivuilta kirjoittamalla terminaaliin "man date".

## Päivämäärän saaminen syvempään tarkasteluun

Päivämäärän hankkiminen Bashilla käyttäen komentoa "date" perustuu pääosin järjestelmän kellonaikaan. Voit myös käyttää command substitutionia eli komennon palauttaman arvon käyttöä osana muuta komentoa, kuten esimerkiksi:

```Bash
echo "Tänään on $(date +"%A")"
```

Tämä tulostaisi jotakin seuraavan kaltaista: "Tänään on maanantai". Voit myös muuttaa järjestelmän kellonaikaa ja päivämäärää käyttäen komentoja "date --set" ja "date --date".

## Katso myös

- Bashin manuaalisivut
- Command substitution Bashissa (englanniksi) https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html#Command-Substitution