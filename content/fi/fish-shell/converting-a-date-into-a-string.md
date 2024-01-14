---
title:    "Fish Shell: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Monissa Fish Shell -sovelluksissa saatetaan joutua muokkaamaan päivämääriä tekstimuotoon. Tämä voi olla tarpeellista esimerkiksi tietojen tallentamiseksi tiedostoon tai niiden näyttämiseksi ruudulla tekstipohjaisessa käyttöliittymässä. Tässä blogikirjoituksessa opimme, miten päivämäärät voidaan muuntaa merkkijonoksi Fish Shellillä.

## Miten näin tehdään

Fish Shellillä päivämäärän muuttaminen merkkijonoksi on helppoa ja nopeaa. Tämä tapahtuu `string` -komennon avulla, joka toimii parametrien avulla annetun päivämäärän kanssa.

```
Fish Shell # String -d "2021-02-15" --date "%x"      
15/02/2021
```

Tässä esimerkissä muutimme päivämäärän merkkijonoksi `"%x"` -parametrin avulla, joka vastaa suomalaisessa päiväyksessä käytettyä muotoa. Voit muuttaa merkkijonon muotoa halutessasi käyttämällä muita parametreja.

```
Fish Shell # String -d "2021-02-15" --date "%B %e, %Y"     
February 15, 2021
```

## Syväsukellusta

Fish Shellin `string` -komennossa on monia muitakin hyödyllisiä parametreja päivämäärän muokkaamiseen. Voit esimerkiksi lisätä päivämäärään aikamääreen käyttämällä `--date` -parametria ja antamalla aikamääreelle `%T` -muodon.

```
Fish Shell # String -d "2021-02-15" --date "%x@%T"     
15/02/2021@00:00:00
```

Lisäksi voit käyttää `--local-time` -parametria, joka palauttaa päivämäärän nimen ja ajanformaatin mukaisen ajan.

```
Fish Shell # String -d "2021-02-15" --date "%x@%T" --local-time     
15/02/2021@14:00:00
```

## Katso myös

- [Fish Shell kotisivu] (https://fishshell.com/)
- [Fish Shell dokumentaatio] (https://fishshell.com/docs/current/)

Tässä blogikirjoituksessa opimme, miten päivämäärät voidaan muuntaa merkkijonoksi Fish Shellillä ja millaisia parametreja tähän tarkoitukseen voidaan käyttää. Fish Shellin yksinkertainen ja selkeä `string` -komento tekee päivämäärän muokkaamisesta helppoa ja nopeaa. Toivottavasti tämä kirjoitus auttoi sinua ymmärtämään tätä toimintoa paremmin!