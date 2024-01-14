---
title:                "PHP: Merkkijonon pituuden löytäminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi etsiä merkkijonon pituutta?

Monissa ohjelmointitehtävissä voi olla tarpeen selvittää kuinka monta merkkiä tai kirjainta sisältää tietty merkkijono. Tämä tieto on tärkeä esimerkiksi jäsenten salasanojen tarkistamisessa tai tekstin formatoinnissa.
 
# Näin teet sen

PHP:ssa on sisäänrakennettu funktio `strlen()`, joka palauttaa merkkijonon pituuden. Alla on esimerkkikoodi ja sen tuloste:

```PHP
<?php
$merkkijono = "Tämä on esimerkki";
echo strlen($merkkijono);
```

Tulostus: 18

# Syvemmälle merkkijonon pituuteen

Merkkijonon pituuden laskeminen ei välttämättä ole aina yksinkertaista. Esimerkiksi jotkin merkit, kuten aakkoset eri kielistä, voivat olla tavallista pidempiä ja vaikuttaa siten merkkijonon pituuteen. Lisäksi merkkijonon pituuden laskeminen voi olla haastavampaa monimutkaisten tietorakenteiden, kuten moniulotteisten taulukoiden, yhteydessä.

On myös tärkeää huomata, että `strlen()` -funktio mittaa merkkijonon pituuden merkeissä eikä sanoissa. Jos haluat laskea sanamäärän, sinun tulee käyttää esimerkiksi `str_word_count()` -funktiota.

# Katso myös

- [`strlen()` -funktion dokumentaatio](https://www.php.net/manual/en/function.strlen.php)
- [`str_word_count()` -funktion dokumentaatio](https://www.php.net/manual/en/function.str-word-count.php)
- [5 tapaa laskea PHP:ssa merkkijonon pituus](https://www.codementor.io/@ammach/5-ways-to-calculate-the-length-of-a-string-in-php-g8b1d7zm9)
- [Merkkijonon käsittely PHP:ssa](https://www.php.net/manual/en/book.strings.php)