---
title:                "Fish Shell: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Kuinka usein olet joutunut käsillä ollessasi päivämäärien kanssa? Olet varmaan pian tullut siihen huomaamiseen, kuinka kätevää olisi saada ne muunnettua helposti merkkijonoiksi. Tätä varten Fish Shellillä on kätevä tapa muuttaa päivämäärät merkkijonoiksi, mikä helpottaa työskentelyä ja säästää aikaa.

## Kuinka

Fish Shellissä päivämäärien muuttaminen merkkijonoiksi on helppoa. Tarvitset vain `date` komennon, jolla saat nykyisen päivämäärän esimerkiksi muodossa "ma tammi 27 2020". Voit myös mukauttaa tulosteen haluamallasi tavalla käyttämällä komennon eri vaihtoehtoja.

```Fish Shell
$ date
ma tammi 27 2020
$ date +%B
tammikuu
```

Voit myös lisätä haluamiasi tietoja päivämäärään, kuten tunteja, minuutteja ja sekunteja, eri vaihtoehtoja käyttämällä. Esimerkiksi komennolla `date +%H:%M:%S` saat tulostettua nykyisen ajan muodossa "14:37:28".

## Syventyminen

Päivämäärien muuttaminen merkkijonoiksi voi olla hyödyllistä monissa erilaisissa tilanteissa. Esimerkiksi voit käyttää sitä luodessasi tiedostoja tai kansioita ja haluat lisätä päivämäärän nimen loppuun.

Voit myös käyttää `date` komentoa hyödyksi skriptejä tai tehtäviä tehdessäsi. Voit muuntaa päivämääriä merkkijonoiksi ja käyttää niitä päättämään esimerkiksi minkä tiedoston avaat tai minkä komennon suoritat.

On myös hyvä huomata, että `date` komennolla on monia muita vaihtoehtoja ja mahdollisuuksia, joten voit tuunata sen sopivaksi juuri sinun tarpeitasi varten.

## Katso myös

- [Fish Shellin dokumentaatio päivämääränformatoinnista](https://fishshell.com/docs/current/commands.html#date)
- [Fish Shell ohjeet merkkijonointiin ja formatointiin](https://fishshell.com/docs/current/tutorial.html#string-expansion)
- [Tulostepäivämäärän muotoilu Bashilla](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)