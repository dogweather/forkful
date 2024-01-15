---
title:                "Yhdistämällä merkkijonoja"
html_title:           "PHP: Yhdistämällä merkkijonoja"
simple_title:         "Yhdistämällä merkkijonoja"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi yhdistellä merkkijonoja PHP:lla? Yksinkertaisesti sanottuna, merkkijonojen yhdistämisen käyttö voi tehdä koodin kirjoittamisesta helpompaa ja säästää aikaa. Se myös auttaa tekemään koodista helpommin luettavaa ja ymmärrettävää.

## Kuinka tehdä se

Yhdistäminen eli concatenating on yksinkertainen prosessi PHP:lla. Se mahdollistaa eri merkkijonojen yhdistämisen yhdeksi isoksi merkkijonoksi. Voit tehdä tämän käyttämällä yhdistävä operaattoria ".", joka yhdistää merkkijonot toisiinsa. Alla on esimerkki:

```PHP
$etunimi = "Matti";
$sukunimi = "Meikäläinen";

$nimi = $etunimi . $sukunimi;

echo $nimi;
```

Tämä tulostaa: "Matti Meikäläinen". Huomaa, että ennen yhdistämistä muuttujan nimeä ei tarvitse määrittää uudelleen.

Voit myös käyttää yhdistävää operaattoria yhdistäessäsi muuttujia ja tekstiä, kuten esimerkissä alla:

```PHP
$nimi = "Matti";
$tervehdys = "Hei, " . $nimi . "!";
echo $tervehdys;
```

Tämä tulostaa: "Hei, Matti!".

## Syvempi sukellus

Yhdistämisessä on myös muita hyödyllisiä toimintoja PHP:ssa. Voit esimerkiksi lisätä välilyönnin yhdistäessäsi merkkijonoja sananjälkeen. Tämä tehdään lisäämällä välilyönti yhdistävän operaattorin jälkeen, kuten alla olevassa koodissa:

```PHP
$etunimi = "Matti";
$sukunimi = "Meikäläinen";

$nimi = $etunimi . " " . $sukunimi;

echo $nimi;
```

Tämä tulostaa: "Matti Meikäläinen", mutta tällä kertaa välissä on välilyönti.

Voit myös käyttää PHP:n sisäistä funktiota `sprintf()` mahdollistaen monimutkaisempia yhdistämisiä. Tämä mahdollistaa muuttujien sijoittamisen tiettyihin paikkoihin merkkijonossa. Esimerkiksi:

```PHP
$tuote = "kakku";
$maara = 2;

$lause = sprintf("Minulla on %d vaniljakakkua.", $maara);

echo $lause;
```

Tämä tulostaa: "Minulla on 2 vaniljakakkua.".

## Katso myös

- PHP muuttujat: https://www.php.net/manual/en/language.variables.php
- Yhdistävä operaattori: https://www.php.net/manual/en/language.operators.string.php#language.operators.string.concat
- sprintf(): https://www.php.net/manual/en/function.sprintf.php