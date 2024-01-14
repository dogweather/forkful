---
title:    "PHP: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi

Miksi koodata päättymispäivämääriä tulevaisuuteen tai menneisyyteen? Useimmissa web-sovelluksissa on tarpeen käsitellä päivämääriä ja aikoja, ja joskus näihin liittyy myös päivämäärien muokkausta. PHP:llä on helppo ja tehokas tapa laskea päiviä tulevaisuuteen tai menneisyyteen, ja tässä blogikirjoituksessa jaan ohjeet siitä, miten se tehdään.

# Miten tehdä

Jotta voit laskea päivämäärän tulevaisuuteen tai menneisyyteen, tarvitset:
- Tietyn päivämäärän, jonka perusteella lasketaan (esim. tänään)
- Päivien tai kuukausien määrän, joka lisätään tai vähennetään päivämäärästä 
- PHP:n `date()` -funktion 

Esimerkiksi, jos haluat laskea päivän huomiseen, voit käyttää seuraavaa koodia:

```PHP
$huomenna = date("d.m.y", strtotime("+1 day"));
echo $huomenna;
```

Tämä koodi antaa seuraavan tuloksen: `21.09.21`. Sinun tarvitsee vain muokata `+1 day` sen mukaan, kuinka monella päivällä haluat laskea. Voit myös lisätä ja vähentää kuukausia ja vuosia, käyttämällä esimerkiksi `+2 months` tai `-1 year` sisällä `strtotime()`-funktiota.

# Syväsukellus

PHP:n `date()`-funktio antaa paljon mahdollisuuksia muokata päivämääriä. Voit lisätä tai vähentää päiviä, viikonpäiviä, kuukausia ja vuosia haluamallasi tavalla, tai voit käyttää tarkempia parametreja, kuten esimerkiksi `strtotime("+1 week 2 days 4 hours")`.

Voit myös antaa `strtotime()`-funktion toisena parametrina tietyn päivämäärän, jonka suhteen lasketaan. Tämä on hyödyllistä, jos haluat laskea päivämäärän tulevaisuuteen tai menneisyyteen tietyn tapahtuman perusteella.

# Katso myös

- [PHP:n virallinen dokumentaatio päivämäärän laskemisesta](https://www.php.net/manual/en/function.date.php)
- [W3Schools-opas päivämäärän laskemiseen PHP:llä](https://www.w3schools.com/php/php_date.asp)