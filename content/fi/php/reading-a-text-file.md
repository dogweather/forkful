---
title:                "Tekstitiedoston lukeminen"
html_title:           "PHP: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstitiedoston lukeminen tarkoittaa tiedon lukemista tekstimuodossa olevasta tiedostosta. Ohjelmoijat käyttävät tätä toimintoa usein, kun he haluavat käsitellä tai tallentaa tietoa, joka ei ole tietokannassa.

## Kuinka:
```PHP
$file = fopen("tiedosto.txt", "r"); // Avataan tiedosto lukutilassa

if($file){ // Tarkistetaan, että tiedoston avaaminen onnistui

  while(($line = fgets($file)) !== false){ // Luetaan tiedosto rivi kerrallaan, kunnes saavutetaan tiedoston loppu
    echo $line; // Tulostetaan rivi
  }

  fclose($file); // Suljetaan tiedosto

} else{
  echo "Tiedoston avaaminen epäonnistui.";
}
```
Esimerkkitiedosto (tiedosto.txt):
```
Tämä on esimerkkilause.
Tämä on toinen esimerkkilause.
```
Esimerkkituloste:
```
Tämä on esimerkkilause.
Tämä on toinen esimerkkilause.
```

## Syvemmälle:
Tekstitiedoston lukeminen on ollut osa ohjelmointia jo pitkään. Ennen PHP:n suosiota, CGI-skripteissä käytettiin usein Shell-skriptejä tekstitiedostojen lukemiseen ja käsittelyyn. Toinen vaihtoehto tekstitiedoston lukemiseen on käyttää PHP:n file()-funktiota, joka palauttaa tekstitiedoston sisällön taulukkona, jossa jokainen rivi on omassa indeksissään.

## Katso myös:
- PHP:n virallinen dokumentaatio tiedostojen käsittelyyn liittyen: https://www.php.net/manual/en/ref.filesystem.php
- Tiedostojen käsittely Shell-skripteissä: https://www.shellscript.sh/tips/file_read.html