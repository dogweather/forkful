---
title:                "PHP: Tekstitiedoston kirjoittaminen"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kuka tahansa haluaisi kirjoittaa tekstitiedoston PHP:llä? Vaikka tämä tehtävä voi tuntua yksinkertaiselta, se on tärkeä taito kaikille php-ohjelmoijille. Tekstitiedostojen käyttö on yleistä PHP-sovelluksissa, oli sitten kyseessä lokitiedostot, käyttäjän syötteen tallentaminen tai jopa tietokannan varmuuskopioiden tallentaminen tekstitiedostoihin.

## Miten
Aloitetaan yksinkertaisella esimerkillä. Tässä käytämme fopen-funktiota luomaan uuden tekstitiedoston ja kirjoitamme siihen sisältöä fput-funktiolla. Lopuksi suljemme tiedoston fclose-funktiolla. 

```PHP
$file = fopen("uusi_tiedosto.txt", "w"); //luo uuden tiedoston kirjoitustilassa
$content = "Tervetuloa lukemaan minunkirjoitettu blogipostaus!"
fwrite($file, $content); //kirjoittaa sisältöä tiedostoon
fclose($file); //sulkee tiedoston
```
Tämä luo uuden tiedoston nimeltä "uusi_tiedosto.txt" ja tallentaa siihen annetun sisällön.

Voit myös lukea olemassa olevia tekstitiedostoja fread-funktiolla ja käsitellä tiedoston sisältöä haluamallasi tavalla. Esimerkiksi voit tulostaa tiedoston sisällön suoraan if-else lausekkeessa, jotta voit tehdä tarkistuksia. 

```PHP
$file = fopen("omatiedosto.txt", "r"); //avaa tiedoston lukutilassa
if($file) {
    while(($line = fgets($file)) !== false) {
        echo $line; //tulostaa joka rivin tiedostosta
    }
    fclose($file); //sulkee tiedoston
else {
    echo "Tiedoston avaaminen ei onnistunut!";
}
```

Voit myös käyttää file_put_contents-funktiota lyhyempään koodiin tekstitiedoston luomiseen ja sisällön tallentamiseen:

```PHP
$content = "Tervetuloa uusi lukija!";
file_put_contents("tervetuloa.txt", $content); //luo uuden tiedoston ja kirjoittaa sisällön samalla rivillä
```

## Syventymistä
Syventääksemme hieman enemmän tekstitiedostojen kirjoittamista, voimme käyttää erilaisia fileread-funktioita lukemaan ja käsittelemään tiedoston sisältöä. Esimerkiksi voimme käyttää file_get_contents-funktiota laajentamaan tiedoston sisältöä merkkijonona ja sitten käyttää strpos-funktiota löytääksemme tietyt merkkijonon sisältä. Tämä voi olla hyödyllistä, jos haluat suorittaa erilaisia tarkistuksia tiedostossa olevien tietojen suhteen.

```PHP
$file = file_get_contents("kayttajat.txt"); //tallentaa tiedoston sisällön merkkijonona
$users = explode("\n", $file); //muuttaa merkkijonon arrayksi, jokainen rivi on oma elementti
foreach($users as $user) {
    if(strpos($user, "admin") !== false) {
        echo $user . " on ylläpitäjä!"; //tarkistaa onko admin-tunnus tiedostossa ja tulostaa sen
    }
}
```

## Katso myös
- [PHP:n virallinen ohjelmointiopas](https://www.php.net/manual/en/function.fopen.php)
- [Tietoa tekstitiedostoista Wikipediassa](https://en.wikipedia.org/wiki/Text_file)
- [Tekstitiedostojen käsittely PHP:llä](https://www.w3schools.com/php/php_file.asp)