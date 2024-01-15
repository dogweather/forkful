---
title:                "Verkkosivun lataaminen"
html_title:           "PHP: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat käyttää verkkosivuston tietoja tai sisältöä omassa ohjelmassasi tai sovelluksessasi, sinun täytyy ladata se ensin. Tämä on mahdollista käyttämällä PHP:n sisäänrakennettua toimintoa ladata verkkosivu.

## Miten se tehdään

Lataamiseen liittyvät tärkeimmät PHP-funktiot ovat `file_get_contents` ja `curl`. Alla on esimerkki siitä, miten voit ladata verkkosivun käyttämällä `file_get_contents` -funktiota ja tulostaa sen sisällön:

```PHP
$url = "https://www.example.com";
$html = file_get_contents($url);
echo $html; 
```

Ja tässä on vastaava esimerkki käyttämällä `curl` -funktiota:

```PHP
$url = "https://www.example.com";
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$output = curl_exec($ch);
curl_close($ch);
echo $output;
```

Nämä esimerkit olettaa, että PHP:llä on oikeudet ladata verkkosivu. Jos haluat ladata verkkosivun, joka vaatii käyttäjänimiä ja salasanoja, voit käyttää `curl` -funktiota muuttamalla `curl_setopt` -kohtia.

## Syventävä sukellus

`file_get_contents` ja `curl` -funktioiden lisäksi on olemassa muita tapoja ladata verkkosivuja PHP:llä. Voit esimerkiksi käyttää `fopen` ja `fgets` -funktioita avataksesi yhteyden verkkosivulle ja käsitelläksesi sen sisältöä rivittäin. Voit myös asettaa erilaisia asetuksia `curl` -funktioon, kuten käyttäjänimiä ja salasanoja tai otsikoita, jotta voit ladata tiettyjä verkkosivuja.

## Katso myös

- [PHP:n virallinen dokumentaatio](https://www.php.net/manual/en/function.file-get-contents.php)
- [Curlin viralliset sivut](https://curl.haxx.se/)
- [Stack Overflow - Lataa sivu PHP:llä](https://stackoverflow.com/questions/3809763/download-an-html-page-using-php/3809792#3809792)