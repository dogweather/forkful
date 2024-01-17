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

## Mitä & Miksi?

Lataaminen tarkoittaa yksinkertaisesti tiedoston tai verkkosivun hakemista internetistä omalle laitteelle. Tämä on yleinen käytäntö ohjelmoinnissa, koska se antaa mahdollisuuden käsitellä ja manipuloida tietoja paikallisesti. Esimerkiksi web-sivun sisältöä voidaan muokata ja näyttää uudelleen muulla tavoin.

## Kuinka:

```PHP
<?php
// Lataa web-sivun sisältö
$web_sisalto = file_get_contents("https://www.example.com");

// Tulosta sisältö
echo $web_sisalto;
?>
```

Tuloste:

```HTML
<!DOCTYPE html>
<html>
<head>
<title>Esimerkki</title>
</head>
<body>
<h1>Tervetuloa</h1>
<p>Tämä on esimerkkisivu.</p>
</body>
</html>
```

## Deep Dive

Lataamisen historia juontaa juurensa internetin alkuvaiheisiin, jolloin höyhenenkevyet tiedostot siirrettiin FTP-protokollan avulla. Nykyään ladattavat tiedostot voivat olla paljon monimutkaisempia ja sisältää koodia, muotoiluja ja multimediaa. PHP:n file_get_contents()-funktio helpottaa verkkosivun lataamista ja sen sisällön käsittelyä.

Vaihtoehtoisesti, JSON-formaatissa olevan datan lataamiseen voidaan käyttää file_get_contents():in lisäksi myös file_put_contents()-funktiota. Tässä tapauksessa tarvitaan myös urlencode()-funktio datan koodaamiseen.

## Katso myös:

W3Schools: https://www.w3schools.com/php/func_filesystem_file_get_contents.asp