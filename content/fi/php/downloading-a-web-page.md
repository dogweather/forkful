---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Verkkosivun lataaminen on prosessi, jossa pyydetään ja noudetaan sisältöä internetistä paikalliseen käyttöön. Ohjelmoijat tekevät tämän esimerkiksi hakukoneiden indeksointia tai web scraping -toimintoja varten.

## Miten tehdä:
Seuraava koodi näyttää, kuinka ladata verkkosivu PHP:n avulla:
```PHP
<?php
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://www.example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$output = curl_exec($ch);
curl_close($ch);    
print_r($output);
?>
```
Tämä koodi lataa 'https://www.example.com' -sivun sisällön ja tulostaa sen.

## Syvä sukellus
### Historiallinen yhteys
Verkkosivujen lataus alkoi, kun internetistä tuli hajautettu resurssien lähde. PHP, joka julkaistiin ensimmäisen kerran vuonna 1995, on jo pitkään ollut yksi tämän toiminnallisuuden toteuttavista kielistä.

### Vaihtoehdot
On olemassa erilaisia tapoja ladata verkkosivuja PHP:n kanssa. `file_get_contents` on yksinkertainen ja tehokas tapa, jos haluat ladata koko verkkosivun sisällön.

```PHP
<?php
$homepage = file_get_contents('http://www.example.com/');
echo $homepage;
?>
```
### Toteutuksen yksityiskohdat
PHP:n `curl` -kirjasto mahdollistaa monimutkaisemmat verkkosivujen latausoperaatiot, kuten otsikoiden määrittämisen, evästeiden käsittelyn ja jopa SSL-sertifikaattien tarkistuksen.

## Katso myös
* PHP Manual: curl ([link](https://www.php.net/curl))
* PHP Manual: file_get_contents ([link](https://www.php.net/manual/en/function.file-get-contents.php))
* W3Schools PHP Tutorial: File Open/Read/Close ([link](https://www.w3schools.com/php/php_file_open.asp))