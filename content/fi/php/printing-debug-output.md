---
title:    "PHP: Tulostaminen virheenjäljitysplitööjä"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat käyttää debuggaus tulosteita ohjelmoidessa PHP:llä? Debuggaus tulosteilla voit helposti tarkistaa koodin suorituksen ja tunnistaa mahdolliset virheet. Ne myös auttavat ymmärtämään miten koodi toimii ja mihin kohtaan se mahdollisesti kaatuu.

## Miten

Debuggaus tulosteita voidaan luoda helposti käyttämällä PHP:n "echo" tai "print_r" -toimintoja. Näitä toimintoja käytetään tulostamaan muuttujien, taulukoiden ja muiden arvojen sisältö terminaaliin tai nettisivulle. Seuraavassa esimerkissä näytämme, miten "echo" -toimintoa voidaan käyttää tulostamaan muuttujan arvo:

```PHP
$numero = 42;
echo "Muuttujan arvo on: " . $numero;
```

Tämän koodin suorituksen jälkeen terminaaliin tulostuu: "Muuttujan arvo on: 42".

Jos haluat tarkastella taulukoiden sisältöä, voit käyttää "print_r" -toimintoa seuraavasti:

```PHP
$taulukko = array("omena", "banaani", "mansikka");
print_r($taulukko);
```

Tämän koodin suorituksen jälkeen terminaaliin tulostuu taulukon sisältö:

Array
(
    [0] => omena
    [1] => banaani
    [2] => mansikka
)

## Syväsukellus

Debuggaus tulosteet eivät ole pelkästään hyödyllisiä virheiden tunnistamisessa, vaan ne voivat myös auttaa ymmärtämään koodin suoritusta ja löytämään mahdollisia pullonkauloja. Voit käyttää niitä esimerkiksi silloin, kun haluat tarkistaa tietokannasta haettujen tietojen sisältöä tai selvittää, miksi tietty lohko koodista ei toimi odotetusti.

On myös hyvä muistaa, että debuggaus tulosteet tulisi poistaa koodista ennen kuin se siirretään tuotantoon. Ne eivät vain hidasta koodin suoritusta, mutta voivat myös paljastaa arkaluonteisia tietoja, kuten tietokannan käyttäjänimiä ja salasanoja.

## Katso myös

- [PHP:n virallinen dokumentaatio debuggaus tulosteista](https://www.php.net/manual/en/function.echo.php)
- [Debuggaus tulosteiden käyttö WordPressissä](https://developer.wordpress.org/plugins/debugging/debugging-output/)