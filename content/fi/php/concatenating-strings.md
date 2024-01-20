---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon liittämistä yhdeksi. Ohjelmoijat tekevät tämän muodostaakseen dynaamisia ja mukautettuja merkkijonoja.

## Näin se toimii:

PHP:ssä merkkijonojen yhdistäminen on helppoa. Tätä varten käytetään pistettä (.) -operaattoria.

```PHP
$string1 = "Moi ";
$string2 = "maailma!";
$finalString = $string1 . $string2;
echo $finalString; // Tulostaa: "Moi maailma!"
```

Huomaa, että voit myös yhdistää merkkijonoja suoraan:

```PHP
echo "Moi " . "maailma!"; // Tulostaa: "Moi maailma!"
```

## Syvällisempi tarkastelu:

Merkkijonojen yhdistäminen PHP:ssä on peräisin C-ohjelmointikielestä, joka on PHP:n perustana. Sen sijaan, että käyttäisimme '+', kuten JavaScript tai C#, me käytämme '.'.

On useita tapoja tehdä merkkijonojen yhdistäminen PHP:ssä. Yllä olevassa esimerkissä olemme käyttäneet pistettä, mutta voimme myös käyttää .= operaattoria tehdäksemme saman:

```PHP
$string1 = "Moi ";
$string1 .= "maailma!";
echo $string1; // Tulostaa: "Moi maailma!"
```
Näiden lähestymistapojen välillä ei ole suorituskyvyn eroja, joten valitse se, joka sopii parhaiten tarpeisiisi ja tyylisiisi.

## Katso myös:

- PHP:n virallinen dokumentaatio merkkijonojen yhdistämiseksi: https://www.php.net/manual/en/language.operators.string.php
- Stack Overflow -keskustelu merkkijonojen yhdistämisen eri tavoista PHP:ssä: https://stackoverflow.com/questions/3960853/concatenate-two-strings-in-php