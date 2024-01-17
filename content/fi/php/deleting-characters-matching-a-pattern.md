---
title:                "“Mallia vastaavien merkkien poistaminen”"
html_title:           "PHP: “Mallia vastaavien merkkien poistaminen”"
simple_title:         "“Mallia vastaavien merkkien poistaminen”"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkien poistaminen, jotka vastaavat jotakin kategoriaa, on tavallinen ohjelmointitekniikka, jota käytetään tietynlaisten merkkijonojen käsittelyyn. Tämä auttaa ohjelmoijia löytämään ja korvaamaan tarpeettomat tai virheelliset merkit tietyissä tapauksissa.

## Kuinka tehdä:

PHP:ssa merkkien poistaminen, jotka vastaavat tiettyä kaavaa, voidaan tehdä käyttämällä ```preg_replace()``` -funktiota. Alla olevassa esimerkissä näytämme, kuinka poistaa välilyönnit merkkijonosta ja tulostaa uuden merkkijonon ilman niitä:

```
$merkkijono = "Tämä on esimerkkilause.";
$uusi_merkkijono = preg_replace('/\s+/', '', $merkkijono);

echo $uusi_merkkijono; // Tulostaa "Tämäonesimerkkilause."
```

Vaihtoehtoisesti voimme myös käyttää ```trim()``` -funktiota, joka poistaisi välilyönnit merkkijonon alusta ja lopusta. Alla olevassa esimerkissä näytämme, kuinka tämä toimii:

```
$merkkijono = '		Tämä on esimerkkilause.		';
$uusi_merkkijono = trim($merkkijono);

echo $uusi_merkkijono; // Tulostaa "Tämä on esimerkkilause."
```

## Syvällinen sukellus:
Merkkien poistamisella on pitkä historia ohjelmoinnissa ja sitä on käytetty monissa eri kielissä ja ympäristöissä. Joissakin tapauksissa sitä käytetään myös suorituskyvyn parantamiseen, jos merkkijonon käsittely on välttämätöntä. PHP:ssa on monia muita funktioita, jotka voivat auttaa merkkien käsittelyssä, kuten ```str_replace()```, ```substr()``` ja ```str_split()```.

PHP:ssa on myös mahdollista käyttää erilaisia säännönmukaisia lausekkeita merkkeihin liittyvien kriteerien määrittämiseksi ja poistamiseksi. Tämän avulla ohjelmoijat voivat luoda monimutkaisia skriptejä merkkien käsittelyyn, jotka voivat auttaa heitä korjaamaan tai tarkastelemaan tiettyjä virheitä merkkijonoissa.

## Katso myös:
- [PHP:n virallinen dokumentaatio merkkien poistamisesta](https://www.php.net/manual/en/function.preg-replace.php)
- [RegExr - ilmaisinpohjainen työkalu säännönmukaisille lausekkeille](https://regexr.com/)
- [PHP: Merkkijonon muokkaaminen ja haku -opas YouTube-videona](https://www.youtube.com/watch?v=Fkn96fVUiKI)