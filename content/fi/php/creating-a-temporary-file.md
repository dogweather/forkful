---
title:                "PHP: Väliaikaisen tiedoston luominen"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi
Temporary-tiedostoilla on monia käyttötapoja PHP-ohjelmoinnissa, kuten tallentamalla väliaikaisesti käyttäjän lähettämiä tiedostoja ennen niiden lopullista tallentamista tai luomalla väliaikaisia muuttujia.

## Kuinka tehdä
```PHP
// Luo väliaikainen tiedosto
$tempFile = tempnam(sys_get_temp_dir(), 'tmp');
echo $tempFile; // Tulostaa esimerkiksi "/tmp/tmp5c7bgv"

// Tallenna tiedostoon
file_put_contents($tempFile, 'Hello World!');

// Lue tiedosto
echo file_get_contents($tempFile); // Tulostaa "Hello World!"

// Poista tiedosto
unlink($tempFile);
```

## Syvällinen tarkastelu
Temporary-tiedostojen luominen tapahtuu tempnam-funktion avulla. Se hyödyntää käyttöjärjestelmän väliaikaishakemistoa ja luo ainutlaatuisen tiedostonimen antamasi etuliitteen perusteella. Tämän jälkeen voit käsitellä tiedostoa kuten mitä tahansa normaalia tiedostoa, kuten tallentaa ja lukea siitä tietoja. On myös hyvä muistaa poistaa tiedosto, kun se ei enää ole tarpeellinen.

## Katso myös
- [PHP tempnam-dokumentaatio](https://www.php.net/manual/en/function.tempnam.php)
- [PHP temporary files tutorial](https://www.tutorialrepublic.com/php-tutorial/php-file-uploading.php) (englanniksi)
- [PHP:n sys_get_temp_dir-funktio](https://www.php.net/manual/en/function.sys-get-temp-dir.php)