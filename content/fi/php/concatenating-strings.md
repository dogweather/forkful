---
title:    "PHP: Merkkijonojen yhdistäminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
Tervetuloa lukemaan tämän päivän blogikirjoitusta! Tänään opimme kaikki tärkeät asiat PHP:n merkkijonojen yhdistämisestä. Tulet huomaamaan, että tämä taito on erittäin hyödyllinen monissa tilanteissa. Joten aloitetaanpa!

## Kuinka
PHP:ssä merkkijonojen yhdistäminen tapahtuu yksinkertaisesti käyttämällä piste-merkkiä (.) kahden merkkijonon välissä. Katso alla olevaa esimerkkiä:

```PHP
$name = "Laura";
echo "Hei, " . $name . "!"; // Output: Hei, Laura!
```

Voit myös yhdistää useita merkkijonoja kerrallaan käyttämällä samaa piste-merkkiä. Esimerkiksi:

```PHP
$firstName = "Katri";
$lastName = "Mäkinen";
echo "Tervetuloa, " . $firstName . " " . $lastName . "!"; // Output: Tervetuloa, Katri Mäkinen!
```

Huomaa myös, että voit yhdistää muita muuttujia tai jopa numeroita merkkijonojen kanssa. Esimerkiksi:

```PHP
$age = 25;
echo "Olen " . $age . " vuotta vanha."; // Output: Olen 25 vuotta vanha.
```

## Syvällisempi sukellus
PHP:ssä merkkijonojen yhdistäminen on nimeltään "string concatenation" ja se on yksi perustaidoista jokaiselle PHP-kehittäjälle. Monissa tapauksissa meidän täytyy yhdistää useita merkkijonoja yhdeksi, jotta voimme tulostaa halutun viestin tai lähettää tietoja tietokantaan.

On myös tärkeää huomata, että PHP:ssä on muitakin tapoja yhdistää merkkijonoja, kuten käyttämällä "sprintf" -funktiota tai "sprintf" -string-muotoilija. Voit lukea lisää näistä vaihtoehdoista dokumentaatiosta.

Yksi tärkeä seikka, johon kannattaa kiinnittää huomiota, on merkkijonojen oikea asettelu. Jos unohdat laittaa piste-merkin yhdistävien merkkijonojen väliin, saat virheen tai tulokset eivät ole odotetut.

Toivottavasti tämä lyhyt artikkeli auttaa sinua ymmärtämään paremmin merkkijonojen yhdistämistä PHP:ssä. Se on yksinkertainen mutta tärkeä taito, jota käytetään jokaisessa PHP-projektissa.

## Katso myös
Lue lisää merkkijonojen yhdistämisestä PHP:ssä virallisesta dokumentaatiosta:

- https://www.php.net/manual/en/language.operators.string.php
- https://www.php.net/manual/en/function.sprintf.php