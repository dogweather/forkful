---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Fish Shell: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Fish Shell - Miten muunnat päivämäärän merkkijonosta

## Mitä & Miksi?

Päivämäärän muuntaminen merkkijonosta tarkoittaa muotoilupäiväajan tai ajanjakson muuntamista tekstistä kalenterikelpoiseen muotoon. Tätä tarvitaan usein ohjelmoinnissa esimerkiksi tietokannassa tallennettujen päivämäärien käsittelyssä.

## Kuinka?

Fish Shellilla voit helposti muuttaa päivämäärän merkkijonosta käyttämällä `date` -komennon `--parse` -vaihtoehtoa. `--format` -vaihtoehdolla voit määrittää haluamasi tarkemman päivämäärän muotoilun.

Esimerkiksi, jos haluat muuttaa merkkijonon `12/10/2021` muotoon `10. joulukuuta 2021`, voit käyttää seuraavaa komentoa:

```
Fish Shell: date --parse %m/%d/%y --format "%d. %B %Y" 12/10/2021
```

Tämä tuottaa seuraavan tulosteen:

```
10. joulukuuta 2021
```

## Syventävä tieto

Päivämäärästä merkkijonosta muuntamisen tarve juontaa juurensa tarpeeseen käsitellä päivämääriä tietokonejärjestelmissä. Aiemmin päivämäärät tallennettiin usein formaattiin, joka ei ollut helposti käsiteltävissä, jolloin tarvittiin tapa muuttaa ne tietokoneelle ymmärrettäväksi muodoksi.

Nykyään on olemassa myös muita vaihtoehtoja päivämäärän muotoilua varten, kuten esimerkiksi käyttää muita ohjelmointikieliä tai hyödyntää erillisiä kirjastoja. Kuitenkin Fish Shell tarjoaa kätevän tavan muuttaa päivämäärä merkkijonosta ilman ylimääräisen koodin kirjoittamista.

## Katso myös

Fish Shellin ja päivämäärän muuntamisen lisäksi voit tutustua muihin hyödyllisiin komentoihin ja toimintoihin Fish Shellin dokumentaatiosta osoitteessa https://fishshell.com/docs/current.

Käyttäjille, jotka etsivät vaihtoehtoja päivämäärän muunnostyökaluille, voidaan suositella esimerkiksi `dateutil` tai `moment.js` -kirjastoja.