---
title:                "Merkkijonojen yhdistäminen"
html_title:           "PHP: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Stringien yhdistäminen (concatenation) tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Ohjelmoijat käyttävät tätä toimintoa esimerkiksi silloin, kun halutaan näyttää käyttäjälle yhdistelmiä eri teksteistä tai arvoista.

## Miten:
```PHP 
$teksti1 = "Tervetuloa ";
$teksti2 = "PHP-maailmaan!";
$kokonainen_teksti = $teksti1 . $teksti2;
echo $kokonainen_teksti;
```

Tulostaa: Tervetuloa PHP-maailmaan!

## Syvempi sukellus:
Stringien yhdistämisen avulla voimme luoda dynaamisia ja yksilöllisiä tekstejä, jotka reagoivat muuttuviin tietoihin. Tämä toiminto on voimassa myös muiden tietotyyppien, kuten numeroiden, kanssa. Stringien yhdistämiseen on olemassa myös muita vaihtoehtoja, kuten käyttää välimerkkiä yhdistettyjen arvojen välissä sijaan pisteoperaattoria (.) tai käyttää sprintf-funktiota.

## Katso myös:
[Dokumentaatio PHP.net-sivustolla](https://www.php.net/manual/en/language.operators.string.php)