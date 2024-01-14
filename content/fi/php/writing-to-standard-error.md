---
title:                "PHP: Kirjoittaminen standardi virheelle"
simple_title:         "Kirjoittaminen standardi virheelle"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Monet PHP-ohjelmoijat saattavat kysyä itseltään, miksi minun pitäisi kirjoittaa virheilmoituksia standardivirheeseen? Yleensä standardivirheen käyttö on hyödyllistä, kun haluamme jättää selvästi luettavissa olevan jäljen ohjelman suorituksen aikana. 

## Kuinka

Tässä on esimerkki PHP-koodista, joka kirjoittaa "Hello World" -viestin standardivirheeseen:

```PHP
<?php
fwrite(STDERR, "Hello World");
```

Suorittaessa tämän koodin, näemme seuraavan tulosteen standardivirheeseen:

```
Hello World
```

Käytämme `fwrite`-funktiota kirjoittaaksemme haluamamme viestin standardivirheeseen. `STDERR` tarkoittaa standardivirhettä ja `fwrite` tarvitsee kaksi argumenttia: tiedoston kahvaa (tässä tapauksessa `STDERR`) ja kirjoitettavaa viestiä.

## Syväsukellus

Kun kirjoitamme viestejä standardivirheeseen, ohjelma ei lopeta suoritusta niiden takia. Tämä tarkoittaa, että voimme käyttää niitä esimerkiksi debuggaustarkoituksiin, ilman että ohjelma keskeytyy. Voimme myös käyttää erilaisia virhekoodeja sisältäviä viestejä, jotta saamme enemmän tietoa mahdollisista ongelmista.

## Katso myös

- [PHP:n virallinen dokumentaatio fwrite-funktiosta](https://www.php.net/manual/en/function.fwrite.php)
- [Standardivirheen käyttäminen debuggaustarkoituksiin](https://stackoverflow.com/questions/4377814/using-stderr-for-debugging-php)
- [PHP:n virallinen dokumentaatio virhekoodeista](https://www.php.net/manual/en/errorfunc.constants.php)