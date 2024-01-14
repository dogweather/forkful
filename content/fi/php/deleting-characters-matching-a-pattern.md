---
title:    "PHP: Mallin mukaisten merkkien poistaminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

PHP-ohjelmoijat voivat haluta poistaa merkkejä, jotka vastaavat tiettyä kaavaa esimerkiksi tietojen käsittelyssä tai käyttäjän syötteen validoinnissa.

## Kuinka

PHP: ssä on käytettävissä useita tapoja deletoimaan merkkejä vastaavat kaavat. Yksi tapa on käyttää `preg_replace()` -funktiota, joka korvaa kaavaa vastaavat merkit tyhjällä merkkijonolla. Alla on esimerkki, jossa poistetaan kaikki numerot merkkijonosta.

```PHP
$text = "Tervetuloa123";
$clean_text = preg_replace('/[0-9]/', '', $text);
echo $clean_text; // tulostaa "Tervetuloa"
```
Toinen tapa on käyttää `str_replace()` -funktiota. Tämä korvaa kaikki kaavan osat halutulla merkkijonolla. Esimerkiksi, jos haluat poistaa kaikki välilyönnit merkkijonosta, voit käyttää seuraavaa koodia:

```PHP
$text = "Tämä on esimerkkilause";
$clean_text = str_replace(' ', '', $text);
echo $clean_text; // tulostaa "Tämäonesimerkkilause"
```

Erittäin tehokas tapa poistaa merkkejä on käyttää `substr()` -funktiota. Tämä leikkaa merkkijonosta halutun määrän merkkejä aloittaen annetusta indeksistä.

```PHP
$text = "Tämä on toinen esimerkkilause";
$clean_text = substr($text, 8);
echo $clean_text; // tulostaa "toinen esimerkkilause", eli leikkaa merkkijonosta ensimmäiset 8 merkkiä pois
```

## Syvällisemmin

Merkkien poistaminen kaavan avulla on hyödyllistä silloin, kun haluat puhdistaa käyttäjän syötettä tai purkaa tietoja. Kaavan avulla voit tarkasti määrittää, mitkä merkit tulee poistaa, ja saat tuloksena puhtaan merkkijonon.

On myös hyödyllistä tietää, että kaavoihin voidaan käyttää erilaisia sääntöjä ja lippuja, jotka helpottavat tiettyjen merkkijonojen poistamista. Esimerkiksi, voit käyttää `i`-lippua, jotta poistetaan kaikki merkit riippumatta siitä, ovatko ne isoja vai pieniä kirjaimia. Lisäksi voit käyttää `g`-lippua, jotta poistetaan kaikki kaavaa vastaavat merkit, ei vain ensimmäistä esiintymää.

## Katso myös

- [PHP: preg_replace() -funktio](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: str_replace() -funktio](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: substr() -funktio](https://www.php.net/manual/en/function.substr.php)