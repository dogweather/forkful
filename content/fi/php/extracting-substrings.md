---
title:                "Aliljonojen erottaminen"
html_title:           "PHP: Aliljonojen erottaminen"
simple_title:         "Aliljonojen erottaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

Mitä & miksi?

Substringien etsiminen on yksi tapa käsitellä merkkijonoja PHP-ohjelmoinnissa. Se tarkoittaa tietyn osan merkkijonosta poimimista ja sen käsittelemistä erillisenä yksikkönä. Tämä voi auttaa ohjelmoijaa käsittelemään ja manipuloimaan tekstin osia tarvittavalla tavalla.

Miksi ohjelmoijat sitten käyttävät substringeja? Syitä on monia, kuten esimerkiksi tekstin analysointi ja käsittely, tiettyjen sanojen tai merkkijonojen etsiminen suuremmasta kokonaisuudesta tai esimerkiksi tekstin muotoiluun liittyvät tarpeet.

Kuinka?

Alla on kaksi yksinkertaista esimerkkiä siitä, kuinka substringeja voi etsiä ja käsitellä PHP:n avulla.

### Esimerkki 1:

```php
$teksti = "Tervehdys maailma!";
$substring = substr($teksti, 8);
echo $substring;
```
Tulos: _maailma_

Kuten esimerkistä nähdään, ```substr()```-funktio ottaa kaksi parametria: ensimmäisenä merkkijonon ja toisena halutun alueen aloituskohdan. Tässä tapauksessa aloitamme merkkijonosta kohdasta 8 ja loppuosa palautuu tuloksena olevaan muuttujaan.

### Esimerkki 2:

```php
$teksti = "123456789";
$substring = substr($teksti, 3, 4);
echo $substring;
```
Tulos: _4567_

Tässä esimerkissä käytetään myös kolmatta parametria, joka määrittää halutun alueen pituuden. Tässä tapauksessa haluamme etsiä neljä merkkiä aloittaen kohdasta 3.

## Syvemmälle

//TODO

## Katso myös

- PHP:n [virallinen dokumentaatio](https://www.php.net/manual/en/function.substr.php)
- Videotutoriaali [substringien käytöstä PHP:ssä](https://www.youtube.com/watch?v=8-4jsK3B9I8)
- [Substringien käyttömahdollisuuksia](https://www.codecademy.com/courses/learn-php/lessons/php-substrings/exercises/substr?action=resume_content_item) käytännössä.