---
title:                "Fish Shell: Kahden päivämäärän vertailu"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit vertailla kahta päivämäärää Fish Shell -ohjelmoinnilla? Ehkä sinulla on ohjelma, jossa tarvitset päivämäärätietoja ja haluat tarkistaa, ovatko ne samat tai kuuluuko toinen päivämäärä toisen välille.

## Miten?

Vertailemalla kahta päivämäärää Fish Shellillä käytetään `date` -toimintoja. Voit käyttää esimerkiksi `date --date="01/01/2021"` -komennolla määrittääksesi haluamasi päivämäärän ja sitten käyttää `date +%s` saadaksesi päivämäärän Unix-aikaleimaksi. Vertaillaaksesi kahta päivämäärää, voit käyttää `-gt`, `-ge`, `-eq`, `-le` tai `-lt` operaattoreita riippuen siitä, haluatko verrata päivämääriä esimerkiksi suurempaan, pienempään tai yhtä suureen.

Seuraava esimerkki vertailee kahta päivämäärää ja tulostaa "true" jos ensimmäinen päivämäärä on ennen toista tai "false" jos päivämäärät ovat samoja tai toinen päivämäärä on ensimmäisen jälkeen:

```Fish Shell
set first_date (date --date="01/01/2021" +%s)
set second_date (date --date="06/01/2021" +%s)

if test $first_date -lt $second_date
    echo "true"
else
    echo "false"
end
```

Tämä koodi tulostaa "true" koska ensimmäinen päivämäärä on ennen toista.

## Syvemmälle

Päivämäärien vertailu Fish Shellissä käyttää Unix-aikaleimoja, joten on tärkeää olla varovainen, kun koodaat ja tarkistaa, että vertailu tapahtuu oikeassa formaatissa. Voit myös käyttää muita `date` -toimintoja, kuten `date -s` muuttaaksesi päivämäärää ja `-d` saadaksesi päivämäärän Unix-aikaleimaksi.

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Unix-aikaleiman selitys](https://www.epochconverter.com/)