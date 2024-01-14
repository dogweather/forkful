---
title:                "PHP: Merkkijonojen yhdistäminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi: Miksi yhdistää merkkijonoja?

Yhdistämisen string-merkkijonon avulla voit yhdistää useita merkkijonoja yhteen ja luoda uuden merkkijonon. Tämä voi olla erittäin hyödyllistä esimerkiksi, kun haluat muodostaa dynaamisen lauseen tai tekstin, joka sisältää muuttujia.

# Miten: Näin yhdistät merkkijonoja PHP:llä

Yhdistäminen merkkijonoja PHP:llä on helppoa käyttämällä "." -merkkiä. Tämä merkki toimii yhdistäjänä eri merkkijonojen välillä ja luo uuden merkkijonon. Katsotaanpa esimerkki:

```PHP
$etunimi = "Matti";
$sukunimi = "Meikäläinen";
$kokonimi = $etunimi . " " . $sukunimi;
echo $kokonimi;
```

Tämä koodinpätkä luo uuden merkkijonon "$kokonimi", jossa yhdistyy aiemmin määritetyt "$etunimi" ja "$sukunimi". Tulosteena saadaan siis "Matti Meikäläinen".

# Syvemmälle: Tietoa merkkijonojen yhdistämisestä

Merkkijonojen yhdistäminen voi olla erittäin hyödyllistä myös, kun haluat muotoilla ja jäsentää tulostettavaa tekstiä. Voit esimerkiksi yhdistää eri muuttujia ja tekstinpätkiä välilyönnillä tai muilla merkeillä, jotta saat luotua selkeämmän ja luettavamman tuloksen. Lisäksi voit käyttää muita PHP:n merkkijonojen manipulointifunktioita, kuten "str_replace" tai "substr", yhdessä yhdistämisen kanssa saadaksesi haluamasi tuloksen.

# Katso myös

- [PHP:n merkkijonojen yhdistäminen](https://www.php.net/manual/en/language.operators.string.php)
- [PHP:n merkkijonojen manipulointifunktiot](https://www.php.net/manual/en/ref.strings.php)