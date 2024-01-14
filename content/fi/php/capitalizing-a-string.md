---
title:                "PHP: Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi joku haluaisi muuttaa merkkijonon ensimmäisen kirjaimen isoksi? Tällainen toiminto voi olla hyödyllinen esimerkiksi tietokantaohjelmoinnissa tai käyttäjän antamien tietojen validoinnissa.

## Kuinka?

Haluamme käyttää PHP-komentoa "ucfirst" muuttaaksemme merkkijonon ensimmäisen kirjaimen isoksi. Tässä esimerkki:

```PHP
$input = "moi maailma!";
echo ucfirst($input);
```

Tulos olisi "Moi maailma!". Voit myös käyttää samaa toimintoa merkkijonon keskellä, esimerkiksi:

```PHP
$input = "hyvää iltaa!";
echo ucfirst($input);
```

Tulos olisi "Hyvää iltaa!".

## Syvemmällä tasolla

"ucfirst" toiminto ei ole rajoitettu vain ensimmäisen kirjaimen muuttamiseen, vaan voit myös muuttaa merkkijonon toisen tai vaikka viimeisen kirjaimen isoksi käyttämällä toista PHP-komentoa, "ucwords". Esimerkiksi:

```PHP
$input = "tämä on esimerkki!";
echo ucwords($input);
```

Tulos olisi "Tämä On Esimerkki!". Tämän toiminnon avulla voit helposti muokata merkkijonoja haluamallasi tavalla.

## Katso myös

- [PHP:n virallinen dokumentaatio](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP.net:n esimerkit](https://www.php.net/manual/en/function.ucfirst.php#example-454)
- [W3Schoolsin opas](https://www.w3schools.com/php/func_string_ucfirst.asp)