---
title:    "PHP: Merkkijonon suurtaaminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi käyttää PHP:ssa merkkijonon suurta kirjainta

On monia syitä, miksi haluat ehkä muuttaa merkkijonon ensimmäisen kirjaimen suureksi, esimerkiksi jos haluat muotoilla käyttäjän antaman nimen oikein tai jos haluat korostaa tiettyjä osia merkkijonosta.

## Miten tehdä se PHP:llä

Merkkijonon ensimmäisen kirjaimen suuren kirjaimen muuttaminen on helppoa PHP:llä. Voit käyttää `ucfirst()` -funktiota, joka muuttaa ensimmäisen kirjaimen suureksi ja palauttaa uuden merkkijonon. Alla on esimerkki koodista, joka käyttää tätä funktiota.

```PHP
<?php
$string = "tämä on esimerkkilause.";
echo ucfirst($string);
```

Tulostus:

```PHP
Tämä on esimerkkilause.
```

Voit myös muuttaa kaikki merkkijonon kirjaimet suuriksi käyttämällä `strtoupper()` -funktiota. Katso esimerkki alla.

```PHP
<?php
$string = "tämä on esimerkkilause.";
echo strtoupper($string);
```

Tulostus:

```PHP
TÄMÄ ON ESIMERKKILAUSE.
```

## Syvemmälle merkkijonon suurten kirjainten taakse

Kuten näette, merkkijonon ensimmäisen kirjaimen suuren kirjaimen muuttaminen PHP:ssa on melko helppoa. Voit myös käyttää muita funktioita, kuten `lcfirst()` muuttaaksesi ensimmäisen kirjaimen pieneksi tai `strtolower()` muuttaaksesi kaikki kirjaimet pieniksi. On myös mahdollista muuttaa tiettyjä kirjaimia suuriksi tai pieniksi käyttämällä `str_replace()` -funktiota.

Katso lisätietoja PHP:n virallisesta dokumentaatiosta merkkijonojen muokkaamisesta.

## Katso myös

- [PHP:n virallinen dokumentaatio merkkijonojen muokkaamisesta](https://www.php.net/manual/en/ref.strings.php)
- [W3Schoolsin tutoriaali merkkijonojen muokkaamisesta PHP:ssa](https://www.w3schools.com/php/php_string.asp)
- [Miten muokata merkkijonoja PHP:ssa - ohjevideo](https://www.youtube.com/watch?v=ul2njh7kqBM)