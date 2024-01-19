---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

PHP:ssa nykyisen päivämäärän hankkiminen tarkoittaa todellisen ajan ja päivän selvittämistä järjestelmästä. Tämä on tarpeellista, koska ohjelmistosovellukset saattavat tarvita aikaleimoja, aikaväliä tai monimutkaisempia ajan manipulointitapoja.

## Näin teet:

PHP:n sisäänrakennettu `date()` funktio on suoraviivainen tapa nykyisen päivämäärän hankkimiseen. Tässä on esimerkki sen käytöstä:

```PHP
<?php

// Tulosta nykyinen päivämäärä muodossa 'pp.kk.vvvv'
echo date('d.m.Y');

?>
```
Kun ajat tämän koodin, se tulostaa nykyisen päivämäärän muodossa 'pp.kk.vvvv', juuri niin kuin esimerkiksi '12.09.2021'.

## Syvällisemmin:

PHP:n `date()` funktio on ollut olemassa kielestä alkaen. Vaikka se on yksinkertainen ja suoraviivainen, samalla se ei ole kovin joustava. Alternatiiveja tämän joustavuuden lisäämiseen ovat esimerkiksi DateTime- ja Carbon-luokat. `date()` käyttää palvelimen aikavyöhykettä oletuksena, ellei muuta ole määritetty. Jotkut saattavat pitää tätä rajoitteena, kun taas toiset näkevät sen hyödyllisenä.

## Katso myös:

- PHP:n virallinen dokumentaatio `date()` funktiolle: https://www.php.net/manual/en/function.date.php

- Hyvä tutorial PHP:n DateTime luokasta: https://www.php.net/manual/en/class.datetime.php

- Dokumentaatio PHP:n Carbon-luokalle: https://carbon.nesbot.com/docs/