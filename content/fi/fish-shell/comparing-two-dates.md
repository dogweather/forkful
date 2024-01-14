---
title:                "Fish Shell: Verrataan kahta päivämäärää."
simple_title:         "Verrataan kahta päivämäärää."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Miksi vertailla kahta päivämäärää? Päivämäärien vertailu voi olla hyödyllistä esimerkiksi sovelluksissa, joissa halutaan tarkistaa, onko jokin tapahtuma tapahtunut ennen tai jälkeen tiettyä päivämäärää, tai vertailla eri ajanjaksoja.

## Miten

Päivämäärien vertailu Fish Shellissa on helppoa ja vaivatonta. Käytämme tähän tarkoitukseen `date` komentoa, joka pystyy muuttamaan annetun päivämäärän UNIX-aikamerkistä haluttuun muotoon. Seuraavassa esimerkissä vertailemme kahta päivämäärää, joista toinen on nykyhetki ja toinen on tuleva päivä.

```Fish Shell
$ date +%s  # Tulostaa nykyhetken UNIX-aikamerkistä
1622467781
$ date -d "Jan 01 2022" +%s  # Tulostaa tulevan päivän UNIX-aikamerkistä
1641022800
```

Käytämme vertailussa komentoja `-ge` ja `-lt`, jotka tarkoittavat "greater than or equal" ja "less than". Näiden avulla voimme vertailla kahta päivämäärää ja nähdä, kumpi niistä on aikaisempi tai myöhäisempi.

```Fish Shell
$ if test (date +%s) -lt (date -d "Jan 01 2022" +%s)  # Jos nykyhetki on ennen tammikuuta 2022
      echo "Tammikuu 2022 ei ole vielä alkanut"
  end
```

Tässä tapauksessa tulostaisimme "Tammikuu 2022 ei ole vielä alkanut", koska nykyhetki (1622467781) on pienempi kuin tulevan päivän UNIX-aikamerkki (1641022800).

## Syvempi sukellus

Päivämäärien vertailu ei rajoitu vain kahteen päivämäärään, vaan voimme myös vertailla useita päivämääriä keskenään. Tähän tarvitaan hieman enemmän koodausta, mutta jälleen kerran Fish Shell tekee sen helpoksi.

```Fish Shell
set dates "2020-01-01 2021-03-15 2019-11-05"  # Luodaan lista päivämääristä
for date in $dates  # Käydään läpi jokainen päivämäärä listassa
    if test (date +%s) -ge (date -d $date +%s)  # Jos nykyhetki on myöhemmin tai sama päivämäärä kuin listassa oleva päivämäärä
        echo $date "on tai ennen nykyhetkeä"
    else 
        echo $date "on jälkeen nykyhetken"
    end
end
```

Tässä esimerkissä tulostamme jokaisen päivämäärän ja tarkistamme, onko se myöhäisempi vai aikaisempi kuin nykyhetki. Voimme myös käyttää muita vertailukomentoja, kuten `-gt` ja `-le`, joiden avulla voimme verrata päivämääriä tarkemmin.

## Katso myös

Näiden esimerkkien lisäksi voit tutustua Fish Shellin `date` komennon dokumentaatioon ja kokeilla muita tapoja vertailla päivämääriä. Voit myös ottaa syvemmän sukelluksen UNIX-aikamerkistä ja sen käytöstä päivämäärien kanssa. Alla muutamia hyödyllisiä linkkejä:

- [Fish Shell: date](https://fishshell.com/docs/current/cmds/date.html)
- [MORE UNIX: Understanding and Using Time on Linux](https://www.linux.com/topic/desktop