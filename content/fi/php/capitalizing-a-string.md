---
title:                "Merkkijonon isoilla kirjaimilla"
html_title:           "PHP: Merkkijonon isoilla kirjaimilla"
simple_title:         "Merkkijonon isoilla kirjaimilla"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon isoilla alkukirjaimilla tarkoitetaan sitä, että muutetaan merkkijonon kaikki kirjaimet isoiksi alkukirjaimiksi. Ohjelmoijat tekevät tämän helpottaakseen tietojen vertailua ja järjestämistä.

## Näin se tehdään:
Tässä on yksinkertainen esimerkki PHP:n `strtoupper()`-funktion käytöstä merkkijonon kaikkien kirjainten muuttamiseksi isoksi:

```PHP
<?php
$alkuperainen = 'tämä on alkuperäinen merkkijono';
$isoilla_kirjaimilla = strtoupper($alkuperainen);
echo $isoilla_kirjaimilla;
?>
```

Kun suoritat tämän koodin, tulostuu seuraava:

```
TÄMÄ ON ALKUPERÄINEN MERKKIJONO
```

## Syvällisempi katsaus
Historiallisesti merkkijonojen isojen kirjainten käyttö tunnettiin jo varhaisissa tietokonekielissä, kuten FORTRAN:ssa. PHP:n `strtoupper()`-funktio hyödyntää näitä historiallisia käsityksiä.

Vaihtoehtona `strtoupper()`-funktiolle on `mb_strtoupper()`, jota käytetään, kun työskennellään monikielisten merkkijonojen kanssa. Tämä on hyödyllistä, kun käsitellään kirjaimia, jotka vaihtelevat kielestä toiseen.

PHP:n `strtoupper` toteutus muuntaa yksinkertaisesti jokaisen merkkijonon merkin vastaavaksi isoksi kirjaimeksi käyttäen sisäistä sanakirjaa, jossa on pieniä ja isoja kirjainpareja.

## Lisätietoja
Lisätietoja php:n `strtoupper`- ja `mb_strtoupper`-funktioista löydät PHP:n virallisesta dokumentaatiosta seuraavista linkeistä:
- [strtoupper()](https://www.php.net/manual/fi/function.strtoupper.php)
- [mb_strtoupper()](https://www.php.net/manual/fi/function.mb-strtoupper.php)