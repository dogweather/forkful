---
title:                "Satunnaislukujen luominen"
html_title:           "PHP: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi luoda satunnaisia numeroita PHP:lla? Satunnaiset numerot ovat tärkeitä monissa ohjelmoinnin sovelluksissa, kuten pelien generoimisessa, salasanojen luomisessa ja testauksessa.

## Miten
Satunnaisia numeroita voi luoda PHP:lla käyttämällä sisäänrakennettua "rand" funktiota. Se ottaa argumenttina parametrin, joka määrittelee halutun numeroiden välillä olevan alueen ja palauttaa satunnaisen numeron tästä alueesta.

```PHP
rand($min, $max);
```

Esimerkiksi, jos haluamme luoda satunnaisen numeron väliltä 1 ja 10, voimme käyttää seuraavaa koodia:

```PHP
$random_number = rand(1,10);
echo $random_number; //tulostaa esimerkiksi 7
```

## Syvempi sukellus
"rand" funktiolla on myös muita hyödyllisiä parametreja, kuten "mt_rand", joka käyttää tehokkaampaa satunnaislukugeneraattoria. Lisäksi PHP tarjoaa myös muita funktioita, kuten "shuffle", jolla voi sekoittaa sattumanvaraisesti taulukoita ja "mt_srand", joka asettaa alkusiementen arvot satunnaislukugeneraattorille.

On myös hyvä huomata, että PHP:n satunnaislukugeneraattorit eivät ole täysin satunnaisia, vaan perustuvat tiettyyn algoritmiin. Tämän vuoksi ne eivät sovellu salauskäyttöön, vaan paras valinta on käyttää PHP:n "random_int" funktiota, joka käyttää tietokoneen käyttöjärjestelmän satunnaislukugeneraattoria.

## Katso myös
- PHP:n virallinen dokumentaatio satunnaislukugeneraattoreista: https://www.php.net/manual/en/function.rand.php
- "random_int" ja "random_bytes" käyttöohjeet: https://www.php.net/manual/en/function.random-int.php
- Tutkimus "todellisesta" satunnaisluku-generoinnista: https://www.geeksforgeeks.org/how-to-generate-large-random-numbers-mathrandom-in-php/