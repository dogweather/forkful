---
title:    "PHP: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa on tarpeellista laskea päivä tulevaisuudessa tai menneisyydessä. Esimerkiksi voit tarvita tietoa tulevista tapahtumista tai laskuja, jotka erääntyvät tulevaisuudessa.

## Näin teet sen

Jos haluat laskea päivämäärän tietyn määrän päiviä tulevaisuudessa tai menneisyydessä, voit käyttää PHP:n *date* -funktiota. Tämä funktio ottaa ensimmäisenä parametrina päivämääränmuodon ja toisena parametrina halutun päivämäärän muutoksen.

```PHP
<?php
echo date("d.m.Y", strtotime("+7 days")) . "\n";
// Tulostaa tulevan päivämäärän seitsemän päivän päästä
```

Tämä esimerkki käyttää yllä mainittua *date* -funktiota ja *strtotime* -funktiota, joka muuttaa päivämäärän halutun muutoksen perusteella. Voit käyttää tätä konseptia myös menneisyyteen laskemiseen, käyttämällä negatiivisia lukuja muutoksena.

```PHP
<?php
echo date("d.m.Y", strtotime("-1 week")) . "\n";
// Tulostaa päivämäärän viikko sitten
``` 

## Syvempi sukellus

Voit myös tarkentaa laskentaa haluamillasi tarkkuuksilla, esimerkiksi muuttamalla päivien sijaan viikkoja, kuukausia tai vuosia. Voit myös käyttää päivämäärän lisäksi myös tarkempaa aikaa, kuten tuntien tai minuuttien muutoksia.

```PHP
<?php
echo date("d.m.Y H:i", strtotime("+2 weeks +3 days +4 hours +5 minutes")) . "\n";
// Tulostaa päivämäärän kahden viikon, kolmen päivän, neljän tunnin ja viiden minuutin päästä
```

Voit myös käyttää muita päivämäärämuotoja tarpeidesi mukaan, esimerkiksi *dateTime* -muotoa.

## Katso myös

- [PHP:n *date* -funktion dokumentaatio](https://www.php.net/manual/en/function.date.php)
- [PHP:n *strtotime* -funktion dokumentaatio](https://www.php.net/manual/en/function.strtotime.php)
- [Tietoa päivämäärämuodoista PHP:ssä](https://www.php.net/manual/en/datetime.formats.php)