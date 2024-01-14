---
title:                "PHP: Regular expressionsin käyttäminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää regexp:iä
Regulaarisia lausekkeita käytetään useissa eri ohjelmointikielissä, kuten PHP:ssä, tiedon etsimiseen ja muokkaamiseen tekstissä. Ne ovat hyödyllisiä esimerkiksi tietokantojen tiedonhaussa ja datan validoinnissa. Regexp-osaamisella pystyy myös lyhentämään monimutkaisia koodinpätkiä yhteen kohtaan.

## Kuinka käyttää regexp:iä
Regexp-ominaisuus on sisäänrakennettu PHP:hen ja sen käyttö on varsin helppoa. Alla on esimerkki tekstiin X haluamamme muutoksen tekemisestä, jotta se vastaa haluttua formaattia.

```PHP
//Ennen:
$text = 'Tervehesti foorumille!';

//Haluttu muotoilu:
$text = 'Tervehdys, foorumille!';

//Regexp:
preg_replace('/Tervehesti/', 'Tervehdys,', $text);
```

Esimerkin PHP-koodissa käytetään `preg_replace()` funktiota muuttamaan tekstissä X sijaitseva sana "Tervehesti" toiseen muotoon. Regexp-osaaja ottaa huomioon kirjoitusvirheet ja muut mahdolliset vaihtoehdot, jotka vastaavat haluttua muotoilua. Regexp:ille luonteenomaisesti backslash-merkki `\` johtaa ohjausmerkintää ja `/`-merkit ovat käytössä regexpien alku- ja loppumääritteissä.

## Syvemmälle regexp:iin
Regulaariset lausekkeet koostuvat erilaisista elementeistä, joista jokaisella on oma roolinsa. Niiden käytössä tärkeää on myös ymmärtää erilaiset speksit, kuten säännölliset lausekkeet (regex), POSIX-säännölliset lausekkeet ja ereg:s. Ohjetta kannattaa tutkia tarkemmin PHP:n virallisilta sivuilta.

## Katso myös
- [PHP: preg_replace funktion dokumentaatio](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP:n viralliset sivut](https://www.php.net/)