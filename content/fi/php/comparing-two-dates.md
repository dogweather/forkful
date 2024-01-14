---
title:                "PHP: Kahden päivämäärän vertailu"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertailemalla kahta päivämäärää on mahdollista selvittää ajanjaksoja ja päivämäärien välistä eroa. Tämä on erityisen hyödyllistä, kun käsitellään aikaperusteisia tapahtumia tai tehdään laskutoimituksia ajan suhteen.

## Kuinka

Vertaillessa kahta päivämäärää PHP:ssä käytetään usein `strtotime()`-funktiota, joka muuttaa päivämäärän merkkijonoksi. Tämän jälkeen merkkijonoihin voi käyttää `strtotime` ja `strtotime`-funktioiden välillä, joka palauttaa päivien tai sekuntien muodostaman eron.

```PHP
$paivamaara1 = "2020-01-01";
$paivamaara2 = "2020-01-15";
$paivienero = strtotime($paivamaara2) - strtotime($paivamaara1);

echo "Päivien ero: " . round($paivienero / (60 * 60 * 24));
```

**Tulostus:**

```
Päivien ero: 14
```

## Syvenny

Päivämäärien vertailu voi olla monimutkaisempaa, kun otetaan huomioon esimerkiksi aikavyöhykkeet ja kesäaikaan liittyvät muutokset. Tarkkojen tulosten saavuttamiseksi kannattaa perehtyä PHP:n virallisiin dokumentaatioihin ja ottaa huomioon myös mahdolliset virheelliset arvot.

## Katso myös

- https://www.php.net/manual/en/function.strtotime.php
- https://www.php.net/manual/en/datetime.diff.php