---
title:    "PHP: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monet PHP-ohjelmoijat joutuvat käsittelemään päivämääriä ja aikoja, ja joskus on tarpeellista muuttaa nämä tiedot merkkijonoksi eri muotoilua varten. Joten tässä blogikirjoituksessa käymme läpi, miten voit muuntaa päivämäärän merkkijonoksi PHP:lla.

## Miten

Seuraavassa on esimerkki koodista, joka muuntaa tänään olevan päivämäärän merkkijonoksi muodossa "DD.MM.YYYY" (päivä.kuukausi.vuosi):

```PHP
<?php
$date = date("d.m.Y");
echo $date;
```

Tämä koodi tuottaa seuraavan tulosteen:

```
17.05.2021
```

Tämä perustuu PHP-funktion "date" käyttöön, joka palauttaa annetun muotoilun mukaisen päivämäärämerkkijonon.

Voit myös muuttaa päivämäärän muita osia, kuten vuoden, kuukauden tai päivän, käyttämällä erilaisia parametreja ja merkkijonoja. Voit löytää täydellisen luettelon kaikista mahdollisista yhdistelmistä PHP:n virallisesta dokumentaatiosta.

## Syventävä sukellus

Nyt saatat kysyä, mitä muita vaihtoehtoja on päivämäärän muotoilemiseen merkkijonoksi kuin "date" -funktio. Tässä muutamia muita vaihtoehtoja, joita voit käyttää:

- DateTime-luokan "format" -metodi: tämä toimii samalla tavalla kuin "date" -funktio, mutta käyttää erilaista syntaksia. Voit lukea lisää tästä PHP:n virallisesta dokumentaatiosta.

- strftime-funktio: tämä tarjoaa enemmän joustavuutta muotoilussa käyttämällä C:n strftime-funktion syntaksia. Voit lukea lisää tästä PHP:n virallisesta dokumentaatiosta.

- Päivämäärän ja ajan alustilavaatimus: tämä on uusi ominaisuus PHP 8:ssa, joka antaa sinulle enemmän valvontaa päivämäärän ja ajan käsittelyssä. Voit lukea lisää tästä PHP:n virallisesta dokumentaatiosta.

Toivottavasti tämä antoi sinulle hyvän yleiskuvan siitä, miten voit muuntaa päivämäärän merkkijonoksi PHP:lla. Muista kuitenkin, että päivämäärän ja ajan käsittelyyn on monia muita käteviä toimintoja, joten kannattaa perehtyä lisää PHP:n dokumentaatioon ja kokeilla erilaisia vaihtoehtoja.

## Katso myös

- PHP:n virallinen dokumentaatio päivämäärän käsittelystä: https://www.php.net/manual/en/datetime.formats.php
- W3Schools-tutoriaali päivämäärän muotoilusta PHP:lla: https://www.w3schools.com/php/func_date_format.asp
- Päivämäärän ja ajan hallintaopas: https://code.tutsplus.com/tutorials/dates-and-times-an-ultimate-guide-for-creating-readable-code-in-php--net-32132