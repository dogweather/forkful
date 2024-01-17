---
title:                "Tiedoston kirjoittaminen"
html_title:           "PHP: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tiedoston kirjoittaminen tarkoittaa yksinkertaisesti tekstin tallentamista tietokoneella olevaan tiedostoon. Tämä voi sisältää esimerkiksi tietokannan tallentamista tai tulostamista tiedostoon. Ohjelmoijat tekevät tätä usein tallentaakseen ja jakaa tietoja, jotka eivät ole staattisia tai jotka tarvitsevat päivittämistä.

## Miten:

<p lang="fi">
  ```PHP
  $tiedosto = fopen("tiedosto.txt", "w");
  fwrite($tiedosto, "Tervetuloa Suomi, PHP on mahtava!");
  fclose($tiedosto);
  
  // Tällä koodilla voit luoda uuden tiedoston nimeltä "tiedosto.txt" ja kirjoittaa siihen tekstin "Tervetuloa Suomi, PHP on mahtava!".

  $sisalto = file_get_contents("tiedosto.txt");
  echo $sisalto;
  
  // Tällä koodilla voit tulostaa tiedoston sisällön sivullesi.
  ```
</p>

Tulostus:
```Tervetuloa Suomi, PHP on mahtava!```

## Syvempi sukeltaminen:

PHP:sta tuli suosittu verkkosivujen kehittämisessä alun perin sen dynaamisen luonteen takia. Ennen PHP:tä, suurin osa sivustoista sisälsi staattista sisältöä, mikä tarkoittaa, että sivut olivat samanlaisia jokaisen käyttäjän kohdalla. PHP mahdollisti sivustojen luomisen, jotka pystyvät luomaan sisältöä eri tavoilla jokaisen sivun lataus kohdalla. Tämä mahdollistaa dynaamisten verkkosivujen luomisen, jotka voivat sisältää käyttäjän toimintoja ja reagoida käyttäjän tekemiin toimintoihin.

On olemassa muita vaihtoehtoja tiedoston kirjoittamiselle, kuten JavaScriptin käyttäminen verkkosivustojen kehittämiseen. JavaScript tarjoaa samanlaisen dynaamisen ominaisuuden kuin PHP, mutta on suunniteltu erityisesti selaimen ympäristöön. PHP on suunniteltu erityisesti serverin ympäristöön, joten sen avulla voidaan tehdä paljon monimutkaisempia toimintoja selaimessa käytettäviin tekniikoihin verrattuna.

## Katso myös:

- [PHP: tiedoston kirjoittaminen](https://www.php.net/manual/en/function.fwrite.php)
- [JavaScriptin perusteet](https://www.w3schools.com/js/js_intro.asp)