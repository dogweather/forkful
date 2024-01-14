---
title:                "PHP: Tekstitiedoston lukeminen"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Jos olet PHP-ohjelmoinnin aloittelija tai haluat oppia lisää, lukeminen on yksi tärkeimmistä taidoista, jotka sinun tulisi hallita. Luku- ja kirjoitustaidon avulla voit käsitellä ja tallentaa tietoa eri tiedostoihin, mikä on välttämätöntä monissa ohjelmoinnin tehtävissä.

## Miten
PHP: lla on useita sisäänrakennettuja toimintoja tiedostojen lukemiseen ja kirjoittamiseen. Tässä esimerkissä käytämme "file_get_contents" -toimintoa lukemaan tiedoston ja "echo" -toimintoa tulostamaan tiedot näytölle.
```PHP
<?php
$file = file_get_contents('tekstitiedosto.txt'); //tallenna tiedoston sisältö muuttujaan
echo $file; //tulosta tiedoston sisältö näytölle
?>
```
Tuloste: Tervetuloa lukemaan minun blogiartikkeliani!

## Syvempää
Tiedoston lukeminen on yksi tärkeimmistä taidoista PHP-ohjelmoinnissa ja siitä on hyötyä monissa tilanteissa. Se voi auttaa sinua käsittelemään ja tallentamaan käyttäjän syöttämiä tietoja, kuten lomakkeiden kautta lähetettyjä viestejä tai käyttäjänimen ja salasanan tarkistamista kirjautumislomakkeella. Voit myös käyttää tiedostojen lukemista verkkosivuilla näyttämään dynaamisesti päivittyviä tietoja, kuten uutisotsikoita.

Toinen tärkeä osa tiedoston lukemista on tiedostopolkujen käyttö. Muista aina määrittää oikea polku tiedostoon, jonka haluat lukea, jotta koodisi toimisi oikein.

## Katso myös
- [PHP:n virallinen dokumentaatio tiedostojen lukemiseen ja kirjoittamiseen](https://www.php.net/manual/en/ref.filesystem.php)
- [Opas tiedostopolun määrittämiseen PHP:ssa](https://www.w3schools.com/php/php_file_paths.asp)
- [Ohjeet tiedostojen lukemisesta ja kirjoittamisesta Javassa](https://www.codecademy.com/articles/reading-files-in-java)