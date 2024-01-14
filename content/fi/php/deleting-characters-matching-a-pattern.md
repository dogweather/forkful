---
title:                "PHP: Kuvion mukaisten merkkien poistaminen"
simple_title:         "Kuvion mukaisten merkkien poistaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa on hyödyllistä pystyä poistamaan merkkejä, jotka vastaavat tiettyä kuvioa ohjelmoinnin aikana. Tämä voi säästää aikaa ja vaivaa manuaaliselta työltä, ja olla tärkeää tiettyjen algoritmien tai datan käsittelyn kannalta.

## Kuinka tehdä

Tehtävän suorittamiseksi PHP:ssä voit käyttää `preg_replace()`-funktiota. Se ottaa parametrina kaksi merkkijonoa, ensimmäisenä olevan kuvion ja toisena korvaavan merkkijonon. Se palauttaa uuden merkkijonon, jossa kaikki kuvioon vastaavat merkit on korvattu annetulla merkkijonolla.

Esimerkiksi, jos haluamme poistaa kaikki välilyönnit merkkijonon lopusta, voimme käyttää seuraavaa koodia:

```PHP
$input = "Tämä on esimerkkiteksti.   ";
$output = preg_replace("/\s+$/", "", $input);
```

Tässä kuviossa `\s+` vastaa yhtä tai useampaa välilyöntiä ja `$` merkkijonon loppua. Lopputuloksena `$output`-muuttujassa on "Tämä on esimerkkiteksti."

## Syvempi sukellus

`preg_replace()`-funktion käyttö voi olla monimutkaisempaa kuin yksinkertainen esimerkkimme. Kuvion käyttäminen regex-säännöissä voi olla hankalaa ja aiheuttaa virheitä. Tärkeää on myös muistaa, että `preg_replace()` ei muuta alkuperäistä merkkijonoa, vaan palauttaa muokatun kopion.

Voit lukea lisää PHP:n regex-ehdoista ja `preg_replace()`-funktion eri parametreistä PHP:n virallisesta dokumentaatiosta.

## Katso myös

- [PHP-pääsivu](https://www.php.net/)
- [preg_replace-dokumentaatio](https://www.php.net/manual/en/function.preg-replace.php)
- [Regex-tutoriaali](https://www.regular-expressions.info/)