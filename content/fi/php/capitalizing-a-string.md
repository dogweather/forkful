---
title:                "PHP: Merkkijonon kirjoittaminen isoilla kirjaimilla"
simple_title:         "Merkkijonon kirjoittaminen isoilla kirjaimilla"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voit joutua käyttämään PHP:ta merkkijonon kapitalisointiin. Yleisin syy on merkkijonon esittäminen isolla alkukirjaimella, mikä voi olla tarpeen esimerkiksi nimien tai otsikoiden kohdalla.

## Miten

Merkkijonon kapitalisointi ei ole vaikeaa PHP:ssa. Se voidaan tehdä yksinkertaisesti käyttämällä built-in funktiota `ucfirst()` tai `ucwords()`. Tässä on esimerkki kultakin:

```PHP
$merkkijono = "tämä on esimerkki merkkijonosta";

// Kapitalisoidaan ensimmäinen kirjain
echo ucfirst($merkkijono); // Tulostaa "Tämä on esimerkki merkkijonosta"

// Kapitalisoidaan jokainen sanan ensimmäinen kirjain
echo ucwords($merkkijono); // Tulostaa "Tämä On Esimerkki Merkkijonosta"
```

Huomaa, että `ucfirst()` kapitalisoi vain merkkijonon ensimmäisen kirjaimen, kun taas `ucwords()` kapitalisoi jokaisen sanan ensimmäisen kirjaimen. Voit myös tehdä oman funktion, joka käyttää `mb_convert_case()` funktiota, jotta saavutetaan parempi tuki monikielisille merkkijonoille.

## Syventävä tieto

Merkkijonon kapitalisointi voi olla hieman haasteellista, kun otetaan huomioon kielen monimuotoisuus. Joissain kielissä kuten saksassa ja venäjässä käytetään erilaisia kirjaimia kuin englannissa. Tämä tarkoittaa, että `ucfirst()` ja `ucwords()` eivät välttämättä toimi toivotulla tavalla kaikilla kielillä.

Myös merkkijonojen kanssa, joissa on joitain erikoismerkkejä tai akronyymejä voi olla haastavaa saavuttaa haluttu kapitalisointi. Tästä syystä on tärkeää ymmärtää merkkijonon rakennetta ja tarvittaessa luoda oma funktio, joka käsittelee merkkijonoja kunkin kielen ja rakenteen mukaan.

## Katso myös

- PHP:n virallinen dokumentaatio `ucfirst()` ja `ucwords()` funktioista: https://www.php.net/manual/en/function.ucfirst.php, https://www.php.net/manual/en/function.ucwords.php
- PHP:n virallinen dokumentaatio `mb_convert_case()` funktiosta: https://www.php.net/manual/en/function.mb-convert-case.php