---
title:                "PHP: Tekstin etsiminen ja korvaaminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi
Monet koodarit joutuvat usein tekemään tekstiä etsimällä ja korvaamalla työtä. Tekstejä voi olla monia syitä, kuten kirjoitusvirheet, vanhentuneet tiedot tai yhteensopimattomuudet. On tärkeää tietää, miten tämä tehdään tehokkaasti ja luotettavasti, jotta koodin ylläpitäminen olisi helpompaa ja sujuvampaa.

# Kuinka tehdä
Käyttämällä PHP: tä voit helposti etsiä ja korvata tekstiä seuraavien funktioiden avulla:

```PHP
str_replace("vanha teksti", "uusi teksti", $muuttuja);
```

Tämä funktio korvaa kaikki esiintymät vanhasta tekstistä muuttujassa uudella tekstillä. Voit myös käyttää säännöllisiä lausekkeita etsimiseen ja korvaamiseen käyttämällä preg_replace-funktiota:

```PHP
preg_replace("/vanha teksti/", "uusi teksti", $muuttuja);
```

Käyttämällä säännöllisiä lausekkeita voit etsiä ja korjata tiettyjä osia tekstistä, mikä on erittäin hyödyllistä esimerkiksi tietokannoista tulevien tietojen muokkaamisessa.

# Syvemmälle
PHP: llä on useita muita tekstin etsimiseen ja korvaamiseen tarkoitettuja funktioita, kuten str_ireplace, joka ei välitä tekstien välisistä eroista, ja preg_match, joka palauttaa tiedon siitä, onko säännöllinen lauseke täsmäänyt tekstin kanssa vai ei. On myös tärkeää muistaa turvallisuusnäkökohdat, kuten suodattaminen ennen tekstin korvaamista, jotta estetään mahdolliset injektiot.

# Katso myös
- PHP:n virallinen dokumentointi tekstien etsimiseen ja korvaamiseen: https://www.php.net/manual/en/function.str-replace.php
- Olioiden korvaaminen PHP: ssä: https://www.geeksforgeeks.org/php-objects-replace-objects/