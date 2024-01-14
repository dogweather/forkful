---
title:    "PHP: Tarkistetaan onko hakemisto olemassa"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa?

Tarkistamalla, onko hakemisto olemassa, varmistamme, että voimme turvallisesti käsitellä tiedostoja ja kansion sisältöä ilman, että aiheutamme virheitä tai häiriöitä. Tämä auttaa myös ohjelmoijaa hallitsemaan tiedostoja ja varmistamaan, että kaikki tarvittavat tiedostopolut ovat olemassa.

## Kuinka tarkistaa hakemiston olemassaolo

On olemassa useita tapoja tarkistaa, onko hakemisto olemassa PHP: ssä. Yksi yksinkertainen tapa on käyttää `is_dir()` -funktiota, joka palauttaa `true` tai `false` riippuen siitä, onko annettu polku olemassa oleva hakemisto vai ei.

```
<?php
$path = "/polku/hakemistoon";
if(is_dir($path)){
    echo "Hakemisto on olemassa!";
} else{
    echo "Hakemisto ei ole olemassa!";
}
```

Toinen vaihtoehto on käyttää `file_exists()` -funktiota, joka ottaa parametrinaan tiedoston tai hakemiston polun ja palauttaa `true` tai `false` sen perusteella, onko kyseinen tiedosto tai hakemisto olemassa.

```
<?php
$path = "/polku/tiedostoon";
if(file_exists($path)){
    echo "Tiedosto on olemassa!";
} else{
    echo "Tiedostoa ei ole!";
}
```

Lopuksi voit myös käyttää `scandir()` -funktiota, joka palauttaa taulukon, joka sisältää kaikki kyseisen hakemiston tiedostot ja alihakemistot. Jos hakemistossa ei ole tiedostoja tai alihakemistoja, taulukko on tyhjä.

```
<?php
$path = "/polku/hakemistoon";
$files = scandir($path);
if(count($files) > 2){
    echo "Hakemistossa on tiedostoja tai alihakemistoja!";
} else{
    echo "Hakemisto on tyhjä!";
}
```

## Syvempi sukellus

Hakemiston olemassaolon tarkistamisella voi olla suuri merkitys ohjelmointiprosessissa, varsinkin jos ohjelmasi käsittelee suuria määriä tiedostoja ja kansioita. Tällä tavalla voit varmistaa, että sovelluksesi toimii ilman virheitä ja estää mahdollisia ongelmia.

## Katso myös
- [PHP:n virallinen dokumentaatio is_dir() -funktiosta](https://www.php.net/manual/en/function.is-dir.php)
- [PHP:n virallinen dokumentaatio file_exists() -funktiosta](https://www.php.net/manual/en/function.file-exists.php)
- [PHP:n virallinen dokumentaatio scandir() -funktiosta](https://www.php.net/manual/en/function.scandir.php)