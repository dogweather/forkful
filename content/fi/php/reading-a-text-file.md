---
title:    "PHP: Tiedoston lukeminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedostoja?

Usein PHP-ohjelmissa joudutaan käsittelemään suuria määriä tietoa, joka on tallennettu tekstitiedostoihin. Näitä tietoja voivat olla esimerkiksi käyttäjien syöttämät lomakkeiden tiedot tai tallennetut logit.

## Miten lukea tekstitiedostoja?

Lukeminen tekstitiedostoja PHP:llä on helppoa käyttämällä `fopen`-funktiota ja `fgets`-funktiota. Alla on esimerkki, miten voit lukea tekstitiedoston sisällön ja tulostaa sen näytölle.

```PHP
$file = fopen("tiedosto.txt", "r");
if ($file) {
    while (!feof($file)) {
        $rivi = fgets($file);
        echo $rivi;
    }
    fclose($file);
}
```

Jos tiedosto sisältää esimerkiksi seuraavanlaisen tekstin:

```
Hello
Hei
Bonjour
```

Yllä olevan koodin suorittamisen jälkeen näytölle tulostuu seuraava tulos:

```
Hello
Hei
Bonjour
```

## Syvällinen sukellus tekstitiedostojen lukemiseen

Mikäli haluat lukea tekstitiedoston sisällön rivi kerrallaan, voit käyttää `fgetc`-funktiota. Lisäksi `file`-funktio avaa tiedoston ja lukee sen sisällön taulukkoon.

```PHP
$file = file("tiedosto.txt");
foreach($file as $rivi) {
    echo $rivi . "<br>";
}
```

Tällöin tulostus on seuraavanlainen:

```
Hello
Hei
Bonjour
```

Tekstitiedoston lukeminen ja sen sisällön käsittely PHP:llä on siis helppoa ja kätevää. Muista aina sulkea tiedosto `fclose`-funktiolla, kun lukuoperaatiot on suoritettu loppuun.

## Katso myös

- [fopen-funktio PHP.netissä](https://www.php.net/manual/en/function.fopen.php)
- [fgets-funktio PHP.netissä](https://www.php.net/manual/en/function.fgets.php)
- [fgetc-funktio PHP.netissä](https://www.php.net/manual/en/function.fgetc.php)