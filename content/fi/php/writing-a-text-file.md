---
title:    "PHP: Tekstitiedoston kirjoittaminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston kirjoittaminen on tärkeä osa PHP-ohjelmoinnin prosessia. Se mahdollistaa tiedon tallentamisen ja jakamisen eri käyttäjien välillä, sekä luo pohjan monille monimutkaisemmille toiminnoille.

## Miten

Tekstitiedoston kirjoittaminen PHP:llä on helppoa. Tässä on esimerkki koodista ja sen tulosteesta:

```PHP
<?php
$file = fopen("tekstitiedosto.txt", "w");
fwrite($file, "Tämä on tekstirivi.");
fclose($file);

echo "Tiedosto on kirjoitettu onnistuneesti!";
```

Tämä ohjelma luo uuden tekstitiedoston nimeltään "tekstitiedosto.txt" ja kirjoittaa siihen tekstin "Tämä on tekstirivi." Kun tiedosto suljetaan, ohjelma tulostaa viestin "Tiedosto on kirjoitettu onnistuneesti!".

## Syvempi sukellus

Tekstitiedostojen kirjoittamisessa on monia eri vaihtoehtoja, kuten tiedostomuodon ja -sijainnin valitseminen. Lisäksi PHP tarjoaa useita muita toimintoja, kuten tiedostojen muokkaamisen ja poistamisen. On tärkeää muistaa hallita tiedostoja asianmukaisesti ja tarkistaa, että kirjoitettava tiedostomuoto on tarkoituksenmukainen käyttötarkoitukseen.

## Katso myös

- [PHP:n tekstitiedostojen kirjoittaminen](https://www.php.net/manual/en/function.fopen.php)
- [Tiedostojen hallinta PHP:llä](https://www.php.net/manual/en/ref.filesystem.php)