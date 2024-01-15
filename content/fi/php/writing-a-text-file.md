---
title:                "Tiedostotiedoston kirjoittaminen"
html_title:           "PHP: Tiedostotiedoston kirjoittaminen"
simple_title:         "Tiedostotiedoston kirjoittaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on tärkeä osa koodaamista ja tekstitiedostojen kirjoittaminen on yksi tapa tallentaa tietoja ja varmistaa niiden pysyvyys. PHP:n avulla voit luoda ja muokata tekstitiedostoja tehokkaasti ja helposti.

## Miten

PHP:lla voit kirjoittaa tekstiä tiedostoon käyttämällä `fwrite()`-funktiota. Ensimmäisenä määritellään tiedosto, johon teksti kirjoitetaan:

```
$myFile = "tekstitiedosto.txt";
```

Sitten avataan tiedosto `fopen()`-funktiolla ja määritellään avattavan tiedoston tila:

```
$handle = fopen($myFile, "w");
```

Seuraavaksi kirjoitetaan haluttu teksti tiedostoon `fwrite()`-funktiolla:

```
fwrite($handle, "Tämä on kirjoitettu teksti");
```

Lopuksi tiedosto suljetaan `fclose()`-funktiolla:

```
fclose($handle);
```

Tämän jälkeen voit avata tekstitiedoston ja nähdä kirjoitetun tekstin.

## Syvällinen sukellus

Syvemmälle tekstitiedoston kirjoittamiseen PHP:lla. `fwrite()`-funktion avulla voit myös kirjoittaa useita rivejä kerralla. Voit esimerkiksi määritellä taulukon, jossa on haluamasi tekstit ja sitten käyttää `foreach()`-silmukkaa kirjoittamaan kaikki tekstit riveittäin tiedostoon:

```
$myTexts = array("Ensimmäinen rivi", "Toinen rivi", "Kolmas rivi");

foreach ($myTexts as $text) {
    fwrite($handle, $text . "\n");
}
```

Tässä esimerkissä `\n` merkitsee rivinvaihtoa ja jokainen taulukon teksti kirjoitetaan omalle rivilleen.

Voit myös käyttää `file_put_contents()`-funktiota, joka avaa tiedoston, kirjoittaa siihen tekstin ja sulkee sen yhdellä komennolla:

```
file_put_contents($myFile, "Tämä on tekstiä");
```

## Katso myös

Tutustu seuraaviin resursseihin saadaksesi lisätietoa PHP:n tekstitiedoston kirjoittamisesta:

- [PHP fwrite() Documentation](https://www.php.net/manual/en/function.fwrite.php)
- [Writing to a Text File with PHP](https://www.w3schools.com/php/php_file_create.asp)
- [PHP File Handling Functions](https://www.tutorialspoint.com/php/php_file_handling.htm)

_**Huomaa:** Muista aina huolehtia tiedostojen oikeuksista ja tietoturvasta ennen kuin kirjoitat tai tallennat tietoja tiedostoihin._