---
title:                "Merkkijonon suurennus"
html_title:           "PHP: Merkkijonon suurennus"
simple_title:         "Merkkijonon suurennus"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
Merkkijonon suuriksi kirjaimiksi muuttaminen tarkoittaa, että jokainen merkki merkkijonossa muuttuu isoiksi kirjaimiksi. Ohjelmoijat tekevät tämän yleensä selkeyttääkseen tai yhdenmukaistaakseen tekstiä, tai helpottaakseen merkkijonojen vertailua.

Miten:
PHP:n strtoupper-funktio voidaan käyttää merkkijonojen suuriksi kirjaimiksi muuttamiseen. Esimerkki:
```PHP
$string = "Tämä on esimerkkiteksti";
echo strtoupper($string);
// Tulostaa "TÄMÄ ON ESIMERKKITEKSTI"
```

Jos haluat muuttaa vain ensimmäisen merkin suureksi kirjaimeksi, voidaan käyttää ucfirst-funktiota. Esimerkki:
```PHP
$string = "tämä on esimerkkiteksti";
echo ucfirst($string);
// Tulostaa "Tämä on esimerkkiteksti"
```

Deep Dive:
Merkkijonon suuriksi kirjaimiksi muuttamista käytettiin ennen vanhaan välttämään koodin aiheuttamia ongelmia eri järjestelmien välillä. Nykyisin se on enemmänkin tapa tehdä tekstistä yhtenäisempää ja selkeämpää. Muita vaihtoehtoja tähän ovat esimerkiksi strtolower, joka muuttaa merkkijonon pieniksi kirjaimiksi, tai mb_convert_case, joka ottaa huomioon myös erikoismerkit ja kielitiedon.

See Also:
- strtoupper-funktion dokumentaatio: https://www.php.net/manual/en/function.strtoupper.php
- Tietoa merkkijonon muokkaamisesta PHP:ssa: https://www.php.net/manual/en/ref.strings.php