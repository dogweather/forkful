---
title:    "PHP: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tapauksissa on tarpeen muuntaa päivämäärä merkkijonoksi. Esimerkiksi, jos haluat näyttää käyttäjälle päivämäärän eri muodossa kuin se on tallennettu tietokantaan. PHP tarjoaa monia eri tapoja muuntaa päivämäärä merkkijonoksi, joten seuraavaksi käymme läpi muutamia niistä.

## Miten

Aloitetaan käyttämällä PHP:n valmista date() -funktiota. Tämä funktio ottaa kaksi parametria: päivämäärämuodon ja aikaleiman. Seuraavassa esimerkissä käytetään muotoa "d.m.Y", joka tarkoittaa päivämäärän päivä.kuukausi.vuosi muotoa:

```PHP
$date = date("d.m.Y", time());

echo $date;
// Output: 20.08.2021
```

Voit myös käyttää strtotime() -funktiota muuntaaksesi päivämäärän merkkijonoksi. Tämä funktio ottaa yhden parametrin, joka on päivämäärä merkkijonona. Seuraavassa esimerkissä käytetään samaa päivämäärämuotoa kuin edellisessä esimerkissä:

```PHP
$date = strtotime("20.08.2021");

echo date("d.m.Y", $date);
// Output: 20.08.2021
```

PHP:n DateTime-luokka tarjoaa myös mahdollisuuden muuntaa päivämäärä merkkijonoksi. Tämä on hyödyllistä erityisesti, jos haluat käsitellä päivämääriä enemmän. Seuraavassa esimerkissä luodaan uusi DateTime-olio, jossa on sama päivämäärä kuin edellisissä esimerkeissä:

```PHP
$date = new DateTime("20.08.2021");

echo $date->format("d.m.Y");
// Output: 20.08.2021
```

## Syvempää sukellusta

Päivämäärän muuntamisen yhteydessä on tärkeää huomioida myös aikavyöhykkeet ja mahdolliset päivämääränmuodostusongelmat. Kannattaa tutustua PHP:n dokumentaatioon ja etsiä itselle sopivin tapa muuntaa päivämäärä merkkijonoksi.

## Katso myös

- [PHP:n date()-funktio](https://www.php.net/manual/en/function.date.php)
- [PHP:n strtotime()-funktio](https://www.php.net/manual/en/function.strtotime.php)
- [PHP:n DateTime-luokka](https://www.php.net/manual/en/class.datetime.php)