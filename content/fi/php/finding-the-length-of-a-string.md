---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "PHP: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Mitä & miksi?

Miksi ohjelmoijat etsivät merkkijonon pituutta? Merkkijonon pituuden määrittäminen on tärkeää, sillä se auttaa ohjelmoijia käsittelemään tietoa ja suorittamaan tarkkoja laskutoimituksia. Esimerkiksi käyttäjän antama teksti voidaan tallentaa merkkijonona ja sen pituus voidaan tarkistaa koodin toimivuuden varmistamiseksi.

# Miten tehdä:

PHP:ssä merkkijonon pituus voidaan helposti määrittää käyttämällä `strlen()` -funktiota. Alla on esimerkkikoodi, jossa koodi ensin tallentaa käyttäjän antaman syötteen muuttujaan ja sitten tulostaa merkkijonon pituuden:

```PHP
<?php
$syote = "Tämä on esimerkki!";
echo strlen($syote); //tulostaa 18
?>
```

# Syväsukellus:

Merkkijonon pituuden määrittäminen on ollut tärkeä osa ohjelmointia jo pitkään, sillä seurataan jo vuosikymmeniä vanhojen koodikieltäisten käsikirjoitusten aikakautta. Nykyään PHP:ssä on monia muita tapoja määrittää merkkijonon pituus, mutta `strlen()` -funktio on edelleen yksi nopeimmista ja luotettavimmista tavoista.

On myös muita tapoja määrittää merkkijonon pituus, kuten `mb_strlen()` -funktio, joka on hyödyllinen, kun käsitellään monikielisiä tekstejä. Lisäksi merkkijonon pituuden voi laskea myös `count()` -funktiolla, jos merkkijono on tallennettu taulukkona.

# Katso myös:

Tässä artikkelissa käsiteltiin vain yksi tapa määrittää merkkijonon pituus PHP:ssä. Tarkempia tietoja ja esimerkkejä löytyy PHP:n virallisesta dokumentaatiosta [täältä] (https://www.php.net/manual/en/function.strlen.php). Jos haluat oppia lisää merkkijonojen käsittelystä PHP:ssä, voit tarkistaa myös [tämän artikkelin] (https://www.php.net/manual/en/language.types.string.php) PHP:n merkkijonojen käsittelystä.