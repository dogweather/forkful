---
title:                "PHP: Satunnaisten lukujen luominen"
simple_title:         "Satunnaisten lukujen luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi PHP-ohjelmoijien kannattaa käyttää satunnaislukugeneraattoria?

Käyttäminen satunnaislukugeneraattoria on olennainen osa PHP-ohjelmointia, sillä se mahdollistaa monien eri toimintojen toteuttamisen. Esimerkiksi pelisovelluksissa tarvitaan satunnaisia numeroita, ja myös tietoturvassa satunnaiset salasanat ovat tärkeitä. Ilman satunnaislukugeneraattoria sovellusten toiminnallisuus voisi olla rajoittunut.

## Kuinka käyttää satunnaislukugeneraattoria PHP:ssä?

Satunnaislukugeneraattoriin pääsee käsiksi käyttämällä PHP:n sisäänrakennettua rand() funktiota. Voit määrittää alku- ja loppuarvot, joiden väliltä satunnaiset numerot generoidaan. Katso alla olevasta koodiesimerkistä, kuinka voit käyttää rand() funktiota ja miten se palauttaa erilaisia satunnaisia numeroita:

```PHP
<?php
    //generoi satunnainen numero välillä 0-100
    $satunnaisluku = rand(0, 100);
    echo "Satunnainen numero välillä 0-100: " . $satunnaisluku . "\n";
    
    //generoi satunnainen kuutiomatriisi välillä 0-5
    $matriisi = array(
        array(rand(0, 5), rand(0, 5), rand(0, 5)),
        array(rand(0, 5), rand(0, 5), rand(0, 5)),
        array(rand(0, 5), rand(0, 5), rand(0, 5))
    );
    echo "Satunnainen kuutiomatriisi välillä 0-5: \n";
    print_r($matriisi);
?>
```

Yllä olevan esimerkin tulostus voisi näyttää esimerkiksi tältä:

```
Satunnainen numero välillä 0-100: 57
Satunnainen kuutiomatriisi välillä 0-5: 
Array
(
    [0] => Array
        (
            [0] => 4
            [1] => 0
            [2] => 5
        )

    [1] => Array
        (
            [0] => 2
            [1] => 2
            [2] => 1
        )

    [2] => Array
        (
            [0] => 1
            [1] => 3
            [2] => 3
        )

)
```

## Syvemmälle satunnaislukugeneraattoriin PHP:ssä

Satunnaislukugeneraattorin pohjana PHP:ssä toimii Mersenne Twister -algoritmi, joka on tunnettu sen laajasta jakautumisesta. Tämä tarkoittaa sitä, että algoritmi generoi satunnaisia numeroita lähes täydellisesti.

On myös tärkeää muistaa, että PHP:n rand() funktio ei ole täysin satunnainen, vaan se perustuu alussa mainittuun algoritmiin. Tämä tarkoittaa, että jos samaan aikaan useat käyttäjät suorittavat saman koodiesimerkin, he saavat todennäköisesti samat satunnaisluvut.

## Katso myös

- [PHP:n rand() funktio](https://www.php.net/manual/en/function.rand.php)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)