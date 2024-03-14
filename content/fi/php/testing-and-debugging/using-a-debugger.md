---
date: 2024-01-26 03:50:47.400388-07:00
description: "Debuggeri on ty\xF6kalu, joka auttaa ohjelmoijia ymm\xE4rt\xE4m\xE4\xE4\
  n, mit\xE4 heid\xE4n koodinsa todellisuudessa tekee sen suorittaessa. Se on suurennuslasi,\
  \ joka\u2026"
lastmod: '2024-03-13T22:44:56.660381-06:00'
model: gpt-4-0125-preview
summary: "Debuggeri on ty\xF6kalu, joka auttaa ohjelmoijia ymm\xE4rt\xE4m\xE4\xE4\
  n, mit\xE4 heid\xE4n koodinsa todellisuudessa tekee sen suorittaessa. Se on suurennuslasi,\
  \ joka\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Debuggeri on työkalu, joka auttaa ohjelmoijia ymmärtämään, mitä heidän koodinsa todellisuudessa tekee sen suorittaessa. Se on suurennuslasi, joka mahdollistaa virheiden—niiden kiusallisten ongelmien, jotka saavat ohjelmamme kaatumaan tai antamaan vääriä vastauksia—tarkastelun ja korjaamisen. Debuggerien käyttö säästää meiltä tunteja tulostuslauseiden ja arvailupelien parissa.

## Miten:
PHP:ssä on interaktiivinen debuggeri nimeltään Xdebug. Näin sitä käytetään.

Ensiksi, varmista että sinulla on Xdebug asennettuna ja konfiguroituna `php.ini`-tiedostossasi:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Seuraavaksi, kirjoita yksinkertainen PHP-skripti, jossa on bugi:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Hups! Tämän pitäisi olla plus, ei miinus
}

$result = add(1, 2);
echo "Tulos on: $result"; // Tuloksen pitäisi olla 3, ei -1
```

Käytä IDE:tä, kuten PhpStormia, asettaaksesi katkaisukohdan klikkaamalla rivinumeroa vieressä. Suorita debuggeri ja seuraa, miten muuttujat muuttuvat kun käyt läpi suoritusta. Kun astut yli `add`-funktion, huomaat että `$result` muuttuu -1:ksi, mikä on odottamatonta.

## Syväsukellus:
Historiallisesti PHP:tä käytettiin ensisijaisesti pieniin skripteihin, ja virheenkorjaus oli lisäämällä `var_dump()` ja `print_r()`-lauseita koodin sekaan. Ajan mittaan, kun PHP tuli keskeiseksi toimijaksi web-kehityksessä, alettiin käyttää monimutkaisempia työkaluja, kuten Xdebug ja Zend Debugger.

Vaihtoehtoja Xdebugille ovat pcov ja phpdbg. Ne tarjoavat erilaisia ominaisuuksia, mutta eivät välttämättä ole yhtä kattavia kuin Xdebug. phpdbg on kevyt, PHP:lle erityinen debuggeri, joka on jaettu PHP:n kanssa alkaen versiosta 5.6, ja pcov on koodikattavuusajuri.

Debuggerin käyttöönotossa muista, että et ikinä jätä debuggeria päälle tuotantopalvelimellasi, koska se voi paljastaa tietoturva-aukkoja ja hidastaa suorituskykyä.

## Katso Myös:
- [Xdebug Dokumentaatio](https://xdebug.org/docs/)
- [PhpStormin Virheenkorjausopas](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net phpdbg:st](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov GitHubissa](https://github.com/krakjoe/pcov)
