---
title:                "Ajan erottaminen merkkijonosta"
html_title:           "PHP: Ajan erottaminen merkkijonosta"
simple_title:         "Ajan erottaminen merkkijonosta"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän muotoilu merkkijonosta on tärkeä taito PHP-ohjelmoijille. Se tarkoittaa päivämäärän erottamista merkkijonosta ja sen kääntämistä päivämäärämuotoon. Tämä on tarpeen esimerkiksi käyttäjän syötteen käsittelyssä tai tietokannan kyselyissä.

## Miten:
```PHP
$date_string = "4/8/2021";
$date = date("j.n.Y", strtotime($date_string));
echo $date;
```

Tuloste: "4.8.2021"

## Syvemmälle:
Päivämäärän muotoilu merkkijonosta on yleinen tehtävä ohjelmoinnissa. Sitä on käytetty jo vuosikymmenien ajan ja se on vasta viime aikoina kehitetty helpommaksi ja monipuolisemmaksi. Vaihtoehtoja päivämäärän muotoiluun ovat esimerkiksi JavaScript-kirjasto Moment.js sekä käsin kirjoittaminen esimerkiksi `explode()` ja `DateTime` -luokkien avulla. Implementaation yksityiskohdat vaihtelevat käytetyn menetelmän mukaan, joten kannattaa tutustua niiden dokumentaatioon.

## Katso myös:
- [PHP:n virallinen dokumentaatio päivämäärän muotoilusta](https://www.php.net/manual/en/datetime.formats.date.php)
- [Moment.js -kirjaston dokumentaatio](https://momentjs.com/docs/)
- [PHP.net -sivuston opetusohjelma päivämäärän muotoilusta](https://www.php.net/manual/en/datetime.formats.date.php#example-4523)