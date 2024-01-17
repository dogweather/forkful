---
title:                "Päiväyksen muuttaminen merkkijonoksi"
html_title:           "PHP: Päiväyksen muuttaminen merkkijonoksi"
simple_title:         "Päiväyksen muuttaminen merkkijonoksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän muuttaminen merkkijonoksi tarkoittaa päivämäärän esittämistä sellaisessa muodossa, joka on helposti luettavissa ohjelmalle. Tätä tarvitaan esimerkiksi tietokantoja käytettäessä, jotta päivämäärä voidaan tallentaa oikeassa muodossa ja hakea tarvittaessa. Tästä syystä kehittäjät muuttavat päivämäärän merkkijonoksi.

## Miten se tehdään?
```PHP
$date = date('d.m.Y'); //tämä muuttaa nykyisen päivämäärän merkkijonoksi muodossa "päivä.kuukausi.vuosi"
echo $date; //tulostaa "15.08.2021"
```

## Syväsukellus
Päivämäärän muuttaminen merkkijonoksi on ollut tärkeä toiminto jo pitkään. Ennen PHP:n kehitystä, päivämäärät tallennettiin millisekunteina, mikä oli hankalaa ja epäkäytännöllistä. Nykyään on olemassa myös muita tapoja muuttaa päivämäärä merkkijonoksi, kuten käyttämällä "DateTime" luokkaa.

## Katso myös
[PHP date() -dokumentaatio](https://www.php.net/manual/en/function.date.php)

[DateTime -dokumentaatio](https://www.php.net/manual/en/class.datetime.php)

[Muuttujien ja funktioiden nimien koodausstandardi (PSR-1)](https://www.php-fig.org/psr/psr-1/)