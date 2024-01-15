---
title:                "Napsaustaminen alamerkkijonoista"
html_title:           "PHP: Napsaustaminen alamerkkijonoista"
simple_title:         "Napsaustaminen alamerkkijonoista"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaisi käyttää alastringien hakua PHP:ssa? Yksinkertaisesti siksi, että se on tehokas tapa käsitellä merkkijonoja ja hakea tiettyjä tietoja suuremmista merkkijonoista.

## Kuinka
Substringien hakeminen PHP:ssa on helppoa ja nopeaa. Käytämme tähän strpos() -funktiota, joka palauttaa merkkijonon tietyn alastringin ensimmäisen esiintymän sijainnin. Tämän jälkeen voimme käyttää substr() -funktiota hakeaksemme haluamamme alastringin.

Esimerkiksi, jos haluamme hakea alistringin "maailma" merkkijonosta "Hello maailma!", voimme käyttää seuraavaa koodia:

```PHP
$merkkijono = "Hello maailma!";
$sijainti = strpos($merkkijono, "maailma");
$alistring = substr($merkkijono, $sijainti, 7);
echo $alistring; // tulostaa "maailma"
```

Voimme myös asettaa alistringin aloituskohdan ja pituuden itse. Esimerkiksi, jos haluamme hakea "maailma" alistringin "Hello maailma!" -merkkijonon lopusta, voimme käyttää seuraavaa koodia:

```PHP
$merkkijono = "Hello maailma!";
$alistring = substr($merkkijono, -7, 7);
echo $alistring; // tulostaa "maailma"
```

## Syvempi sukellus
Hakemalla alastringejä PHP:ssa, voimme myös käyttää erilaisia hakuparametrejä, kuten alistringin sijoittumista merkkijonon sisällä ja alistringin suuruutta. Voimme myös käyttää regex-sääntöjä (regular expressions) alistringin hakemiseen, mikä tekee prosessista vieläkin monipuolisemman.

On kuitenkin tärkeää huomata, että alistringien haku voi aiheuttaa ongelmia, jos käsiteltävänä oleva merkkijono muuttuu. Tällöin sijainti ja pituus, joilla olemme hakenneet alastringiä, eivät enää ole oikein.

## Katso myös
- [PHP strpos dokumentaatio](https://www.php.net/manual/en/function.strpos.php)
- [PHP substr dokumentaatio](https://www.php.net/manual/en/function.substr.php)
- [Regular expressions in PHP](https://www.php.net/manual/en/book.pcre.php) (englanniksi)