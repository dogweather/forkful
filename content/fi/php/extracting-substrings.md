---
title:    "PHP: Alimerkkijonojen erottelu"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen (extracting) on tärkeä osa PHP-ohjelmoinnin oppimista. Substringit ovat osa merkkijonoa, joka on poimittu toisesta merkkijonosta. Tämä on hyödyllistä esimerkiksi, kun haluat hakea ja käsitellä tietoja tietokannasta.

## Näin teet sen

Substringien erotus PHP:ssä on suhteellisen helppoa käyttäen muutamia funktionaalisia työkaluja. Alla olevassa koodiesimerkissä käytämme "explode()" funktiota, joka jakaa merkkijonon erillisiin osiin (tokens) annetun erottimen perusteella. Suorita seuraava koodi ja näet tuloksen, jossa kukin "token" on erotettu omaksi stringikseen.

```PHP
<?php
$string = "Tämä on esimerkki merkkijonosta";
$tokens = explode(" ", $string);
foreach($tokens as $token){
    echo $token . "\n";
}
```

Tulos:

```
Tämä
on
esimerkki
merkkijonosta
```

Voit myös käyttää "substr()" funktiota, joka palauttaa halutun osan merkkijonosta annetun aloitus- ja lopetuskohdan perusteella. Koodiesimerkki näyttää, kuinka voit käyttää tätä funktiota poimimaan tietyn alueen merkkijonosta.

```PHP
<?php
$string = "Tämä on esimerkki merkkijonosta";
$substring = substr($string, 4, 6);
echo $substring; // palauttaa "on esi"
```

## Syvällinen sukellus

On tärkeää ymmärtää, että substringien erotus on yksi keino käsitellä merkkijonoja PHP:ssä. On myös muita tapoja, kuten käyttää "preg_split()" funktiota, joka mahdollistaa regular expression -ilmauksien käytön merkkijonojen erotteluun. On tärkeää tutustua eri vaihtoehtoihin ja löytää itselleen sopivin tapa käsitellä merkkijonoja.

## Katso myös

- [Official PHP substring documentation](https://www.php.net/manual/en/function.substr.php)
- [Explode or split a string in PHP](https://www.geeksforgeeks.org/explode-or-split-a-string-in-php/)
- [PHP Regular Expressions](https://www.w3schools.com/php/php_regex.asp)