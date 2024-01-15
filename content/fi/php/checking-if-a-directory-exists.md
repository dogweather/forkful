---
title:                "Tarkistetaan onko hakemistoa olemassa"
html_title:           "PHP: Tarkistetaan onko hakemistoa olemassa"
simple_title:         "Tarkistetaan onko hakemistoa olemassa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa PHP-ohjelmoijan täytyy tarkistaa, onko tietty hakemisto olemassa. Tämä voi johtua esimerkiksi tarpeesta tallentaa tiedostoja, lukea tiedostoja tai tarkistaa, onko tietty hakemisto käytettävissä jollekin tärkeälle toiminnolle.

## Miten tehdä se

Tässä on yksinkertainen koodiesimerkki, joka näyttää, kuinka tarkistaa, onko hakemisto olemassa:

```PHP
if(file_exists("/polku/hakemisto")) {
    echo "Hakemisto on olemassa!";
} else {
    echo "Hakemistoa ei löytynyt.";
}
```

Koodi käyttää PHP:n sisäänrakennettua `file_exists()`-funktiota, joka tarkistaa, onko annettu tiedosto tai hakemisto olemassa. Jos hakemisto on olemassa, tulostetaan ilmoitus "Hakemisto on olemassa!", muuten tulostetaan "Hakemistoa ei löytynyt.".

Voit myös tarkistaa, onko hakemisto olemassa `is_dir()`-funktion avulla:

```PHP
if(is_dir("/polku/hakemisto")) {
    echo "Hakemisto on olemassa!";
} else {
    echo "Hakemistoa ei löytynyt.";
}
```

Tämä funktio tarkistaa, onko annettu polku hakemisto ja palauttaa boolean-arvon (`true` tai `false`) sen mukaan, mitä se löytää.

## Syvemmälle tutustuminen

`file_exists()`- ja `is_dir()`-funktiot käyttävät molemmat samanlaista logiikkaa tarkistamaan, onko hakemisto olemassa. Ne tekevät käytännössä saman asian, mutta `is_dir()` on hieman tarkempi ja soveltuu erityisesti hakemistojen tarkistamiseen.

On myös hyvä muistaa, että nämä funktiot tarkistavat vain, onko hakemisto olemassa, eikä niiden avulla voi esimerkiksi tarkistaa, onko hakemisto kirjoitussuojattu tai pääsy estetty. Näihin asioihin tulee kiinnittää huomiota sovellusta suunnitellessa.

## Katso myös

- [PHP.net: file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [PHP.net: is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Tarkista, onko tiedosto olemassa - PHP-opas](https://www.php-opas.com/tarkista-onko-tiedosto-olemassa/)