---
title:                "PHP: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeellista laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä, kuten laskut ja laskutus, ohjelmoidut tapahtumat tai muut henkilökohtaiset syynä. PHP:lla voit helposti laskea tulevan tai menneen päivän tietyllä aikavälillä.

## Miten

```PHP
// Laske tuleva päivä 7 päivän kuluttua
echo date('d.m.Y', strtotime("+7 days"));

// Laske menneiden päivien määrä
$päivät = (strtotime('10.08.2020') - strtotime('01.01.2020')) / 86400;
echo "Menneiden päivien määrä: " . $päivät;
```

**Tulos:**

17.08.2020

Menneiden päivien määrä: 222

## Syvällinen sukellus

PHP:n date-funktiossa on hyödyllisiä parametreja, joita voit käyttää tulevan tai menneen päivän laskentaan. Esimerkiksi voit määrittää aikavälin, kuten 1 päivä, 1 viikko tai 1 kuukausi. Voit myös lisätä tai vähentää tietyn ajanjakson nykyisestä päivämäärästä. Syntaksin tarkempi selvittäminen auttaa sinua löytämään oikeat parametrit tarpeidesi mukaan.

## Katso myös

- [PHP:n virallinen dokumentaatio](https://www.php.net/manual/en/function.date.php)
- [W3Schoolsin opas](https://www.w3schools.com/php/func_date_date.asp)
- [PHP:n date-funktion käyttö yhdessä strtotime-funktion kanssa](https://www.php.net/manual/en/datetime.formats.relative.php)