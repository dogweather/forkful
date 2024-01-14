---
title:    "Fish Shell: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi
On usein tarpeellista laskea tietty päivämäärä menneisyydessä tai tulevaisuudessa, esimerkiksi tapahtumien järjestämiseksi tai laskujen maksamiseksi. Fish Shell tarjoaa helpon ja tehokkaan tavan tehdä nämä laskelmat.

## Miten
Fish Shellilla on kätevä `date`-komento, joka mahdollistaa päivämäärän laskemisen tulevaisuudessa tai menneisyydessä. Se ottaa parametreiksi päivämäärän ja halutun ajanmäärän, ja tuloksena saadaan uusi päivämäärä.

```Fish Shell
date --date="2021-05-01 + 1 week"
```
Tämä komento tuottaa seuraavan viikon päivämäärän, eli `2021-05-08`. Voit myös laskea päiviä, kuukausia tai vuosia yksinkertaisesti vaihtamalla aikayksikön kirjaimen.

```Fish Shell
date --date="2021-05-01 + 2 months"
```
Tällä kertaa komento tuottaa päivämäärän `2021-07-01`.

## Syvemmälle
`date`-komennon lisäksi Fish Shellilla on myös muita hyödyllisiä työkaluja päivämäärien käsittelyyn. Esimerkiksi `strftime`-komennolla voit muokata päivämäärän näyttämään haluamallasi tavalla. Voit myös hyödyntää Shellin sisäisiä muuttujia, kuten `%Y` vuoden esittämiseen tai `%m` kuukauden esittämiseen kahdella numerolla.

```Fish Shell
echo (strftime "%Y-%m-%d" (date --date="tomorrow"))
```
Tämä tulostaa huomisen päivämäärän muodossa `2021-04-30`.

## Katso myös
- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/cmds/date.html)
- [Stack Overflow: How to calculate a future date in shell script](https://stackoverflow.com/questions/8903239/how-to-calculate-a-future-date-in-shell-script)
- [Linuxize: Working with Dates in Fish Shell](https://linuxize.com/post/working-with-dates-in-fish-shell/)