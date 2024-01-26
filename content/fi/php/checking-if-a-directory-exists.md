---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:57:41.578171-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Tarkistetaan, olemassaako hakemisto vai ei, joka on PHP:ssä yksinkertainen mutta tärkeä toiminto. Ohjelmoijat tekevät tämän, ettei sovellus kompastu puuttuviin tiedostoihin ja kansioihin, mikä voisi aiheuttaa virheitä tai turvallisuusriskejä.

## How to: - Kuinka tehdään:
```PHP
<?php
$directory = "/var/www/html/my_folder";

// Tarkistetaan, onko hakemisto olemassa
if (is_dir($directory)) {
    echo "Hakemisto on olemassa.";
} else {
    echo "Hakemistoa ei löydy.";
}

// Tulostus: Hakemisto on olemassa. TAI Hakemistoa ei löydy.
?>
```

## Deep Dive - Syväsukellus:
Historiallisesti `is_dir` on ollut PHP:n perustyökalu hakemistojen olemassaolon tarkistamiseen. Alternatiivina `file_exists`-funktio voidaan käyttää, mutta se ei erottele tiedostoja ja hakemistoja. Tiedosto- ja hakemistopolitiikat voivat vaikuttaa tarkistukseen – esimerkiksi oikeudet ja symboliset linkit voivat johtaa harhaanjohtaviin tuloksiin. Tästä syystä on hyvä ymmärtää funktioiden taustalogiikka ja testiprosessit eri ympäristöissä.

## See Also - Katso myös:
- PHP:n virallinen dokumentaatio `is_dir`: https://www.php.net/manual/en/function.is-dir.php
- PHP:n virallinen dokumentaatio `file_exists`: https://www.php.net/manual/en/function.file-exists.php
- Stack Overflow keskusteluja ja esimerkkejä hakemistojen käsittelystä PHP:ssä: https://stackoverflow.com/questions/tagged/php+directory
