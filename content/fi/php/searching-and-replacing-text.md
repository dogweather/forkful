---
title:    "PHP: Etsiminen ja tekstin korvaaminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ohjelmoijat joutuvat tekemään laajoja tekstikorvauksia. Ehkä koodisi sisältää vanhentuneita funktioita tai haluat muuttaa nimiä johdonmukaisuuden vuoksi. Olipa syysi mikä tahansa, PHP tarjoaa tehokkaan työkalun tekstikorvausten tekemiseen.

## Miten

Yksi tapa tehdä tekstikorvauksia PHP:ssa on käyttää str_replace-funktiota. Se ottaa kolme parametria: korvattava teksti, uusi teksti ja lähdeteksti. Alla on esimerkki, jossa korvaamme kaikki "hello"-sanat "hei"-sanalla:

```PHP
$input = "Hello, world!";
$output = str_replace("hello", "hei", $input);
echo $output; // Tulostaa "Hei, world!"
```

Voit myös käyttää regex-säännöllisiä lausekkeita tekstikorvauksissa. Alla oleva esimerkki korvaa kaikki numerot tekstistä tyhjällä merkillä:

```PHP
$input = "Hello 123!";
$output = preg_replace("/[0-9]/", "", $input);
echo $output; // Tulostaa "Hello !"
```

## Syvemmälle

Jos työskentelet paljon tekstin kanssa, saatat joutua vaihtamaan kaikki esiintymät tietyllä rivillä tai tietyn päivämäärän välillä. Tämä voidaan helposti tehdä foreach-silmukan avulla yhdistettynä edellä mainittuihin tekstikorvausmenetelmiin. On myös muita PHP:n sisäisiä funktioita, kuten preg_match, jotka voivat auttaa sinua löytämään ja korvaamaan tiettyjä tekstejä.

## Katso myös

- [PHP:n virallinen dokumentaatio tekstikorvauksista](https://www.php.net/manual/en/function.str-replace.php)
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Codecademy:n interaktiiviset ohjelmointikurssit](https://www.codecademy.com/learn/learn-php)