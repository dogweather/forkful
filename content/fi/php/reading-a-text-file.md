---
title:                "PHP: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

HelsinkiWeb-ohjelmointi blogi

## Miksi

Lukemalla tekstitiedostoja on tärkeää taitoa, jota jokaisen PHP-kehittäjän tulisi hallita. Tekstitiedostoihin tallennetaan usein tärkeitä tietoja, kuten käyttäjien syöttämiä tietoja tai sovelluksen konfiguraatiotietoja. Lukemalla näitä tiedostoja voit käyttää ja hyödyntää niiden sisältöä PHP-sovelluksessasi.

## Kuinka

Tekstitiedostojen lukeminen PHP:lla on yllättävän helppoa. Voit tehdä sen käyttämällä file_get_contents() -funktiota ja antamalla sille tekstifileen polun parametrina. Tämä funktio palauttaa tiedoston sisällön merkkijono-muodossa, joten voit käsitellä sitä kuten tavallista muuttujaa.

```
PHP $file_content = file_get_contents("/polku/tekstitiedosto.txt");
echo $file_content; // tulostaa tiedoston sisällön
```

Voit myös käyttää file() -funktiota, joka luo tiedoston sisällöstä taulukon, jossa jokainen rivi on oma alkionsa.

```
PHP $file_lines = file("/polku/tekstitiedosto.txt");
foreach ($file_lines as $line) {
  echo $line; // tulostaa jokaisen rivin erikseen
}
```

## Syväsukellus

On tärkeää huomata, että file_get_contents() ja file() -funktiot lukevat tiedoston sisällön kokonaan muistiin ennen kuin palaavat sen arvon. Tämä voi olla ongelmallista suurille tiedostoille, jotka saattavat aiheuttaa suorituskykyongelmia tai jopa aiheuttaa sivuston kaatumisen. Tässä tapauksessa suosittelemme käyttämään fopen() ja fread() -funktioita, jotka lukevat tiedoston sisällön pienissä paloissa kerrallaan.

```
PHP
$handle = fopen("/polku/tekstitiedosto.txt", "r");
if ($handle) {
  while (($line = fgets($handle)) !== false) {
    echo $line; // tulostaa jokaisen rivin erikseen
  }
  fclose($handle);
} else {
  echo "Tiedoston avaaminen epäonnistui.";
}
```

## Katso myös

- [PHP:n virallinen tiedostojen käsittely -dokumentaatio](https://www.php.net/manual/en/book.filesystem.php)
- [Tekstitiedostojen kirjoittaminen PHP:lla](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP-ohjelmoinnin perusteet](https://www.w3schools.com/php)
- [PHP-foorumit ja yhteisöt](https://www.php.net/community)