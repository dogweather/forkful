---
title:    "PHP: Merkkijonon pituuden löytäminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi
Laskeminen on yksi tärkeimmistä taidoista ohjelmoinnissa. Strukturoidussa ohjelmoinnissa pitkät merkkijonot ovat yleinen tietotyyppi, ja niiden pituuden laskeminen on tärkeä taito, jota tarvitaan monissa eri käyttötarkoituksissa.

## Miten
```PHP
$merkkijono = "Tämä on esimerkki merkkijonosta";
echo strlen($merkkijono);
```
Tulostaa: 30

Merkkijonon pituuden laskeminen PHP:ssa on helppoa käyttämällä `strlen()` -funktiota. Tämä funktio palauttaa merkkijonon pituuden, eli siinä olevien merkkien määrän.

## Syvemmälle
`strlen()` -funktio laskee merkkijonon pituuden määrittämällä jokaisen merkin ASCII-koodin ja laskemalla niiden summan. Tästä syystä se ei aina anna oikeaa tulosta, jos merkkijono sisältää erikoismerkkejä, kuten ääkkösiä tai erikoismerkkejä.

Toinen tapa laskea merkkijonon pituus on käyttää `mb_strlen()` -funktiota, joka ottaa huomioon myös erikoismerkit ja palauttaa oikean määrän merkkejä.

```PHP
$merkkijono = "Tämä on esimerkki merkkijonosta";
echo mb_strlen($merkkijono);
```
Tulostaa: 30

On myös mahdollista laskea tietyn alueen pituus merkkijonosta käyttämällä `substr()` -funktiota yhdistettynä `strlen()` tai `mb_strlen()` -funktioon.

```PHP
$merkkijono = "Tämä on esimerkki merkkijonosta";
echo strlen(substr($merkkijono, 0, 5));
```
Tulostaa: 5

## Katso myös
- PHP:n virallinen dokumentaatio: https://www.php.net/manual/en/function.strlen.php
- Laskeminen ja merkkijonot: https://www.tutorialrepublic.com/php-tutorial/php-strings.php
- Erikoismerkit ja merkkijonot: https://www.php.net/manual/en/function.mb-strlen.php