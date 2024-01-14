---
title:    "PHP: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["PHP"]
---

{{< edit_this_page >}}

Miksi: Onko sinulla koskaan ollut tarvetta muuttaa merkkijono pieniksi kirjaimiksi PHP-ohjelmointikielessä? Tässä blogikirjoituksessa kerromme, miksi tämä toiminto voi olla hyödyllinen ja miten voit tehdä sen helposti.

Kuinka tehdä: Voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä PHP-funktioita strtolower() ja mb_strtolower(). Tässä esimerkissä käytämme strtolower() -funktiota ja tulostamme pienet kirjaimet:

```PHP
$input = "TÄMÄ ON MERKKIJONO ISOILLA KIRJAIMILLA";
$output = strtolower($input);
echo $output;
```

Tämä tulostaa "tämä on merkkijono isoilla kirjaimilla". Voit myös käyttää mb_strtolower() -funktiota, jos työskentelet eri kielillä, jotka sisältävät ei-latinalaisia merkkejä.

Syvä sukellus: Merkkijonon muuntaminen pieniksi kirjaimiksi voi olla hyödyllistä monissa tilanteissa, kuten tietokantojen käsittelyssä tai tietojen validoinnissa. PHP tarjoaa myös muita hyödyllisiä funktioita, kuten ucfirst(), joka muuntaa vain ensimmäisen kirjaimen isoksi, ja ucwords(), joka muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi. Näiden funktioiden avulla voit muokata merkkijonoja helposti tarpeen mukaan.

Katso myös: Mikäli haluat oppia lisää PHP:n merkkijonojen käsittelystä, suosittelemme tutustumaan PHP:n viralliseen dokumentaatioon sekä seuraaviin oppaisiin:

- PHP:n virallinen dokumentaatio: https://www.php.net/manual/en/function.strtolower.php
- Codecademy:n interaktiivinen kurssi PHP:stä: https://www.codecademy.com/learn/learn-php
- W3Schools:n PHP-opetusohjelmat: https://www.w3schools.com/php/