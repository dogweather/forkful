---
title:                "PHP: Satunnaislukujen luominen"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Random-numeroiden generointi on hyödyllistä ja välttämätöntä monissa ohjelmointiprojekteissa. Se voi lisätä vaihtelevuutta ja arvaamattomuutta sovellukseesi, mikä tekee siitä turvallisemman ja viihdyttävämmän käyttäjille.

## Miten
Random-numeroiden generointi on helppoa PHP:lla hyödyntäen valmiita toimintoja. Voit käyttää esimerkiksi `rand()`-funktiota, joka palauttaa satunnaisen kokonaisluvun annetulta väliltä. Katso alla olevia esimerkkejä ja niiden tulosteita.

```PHP
// Palauttaa satunnaisen kokonaisluvun väliltä 1-10
echo rand(1, 10); // Esim. 7
```

```PHP
// Palauttaa satunnaisen numerojoukon väliltä 1000-9999
echo rand(1000, 9999); // Esim. 4687
```

Kokeile myös `mt_rand()`-funktiota, joka käyttää parempaa algoritmia satunnaislukujen generointiin.

```PHP
// Palauttaa satunnaisen kokonaisluvun väliltä 1-10
echo mt_rand(1, 10); // Esim. 3
```

```PHP
// Palauttaa satunnaisen numerojoukon väliltä 1000-9999
echo mt_rand(1000, 9999); // Esim. 8175
```

## Deep Dive
Satunnaislukujen generointi ei kuitenkaan ole täysin sattumanvaraista. Ohjelmoinnissa käytettävät algoritmit eivät pysty luomaan täysin satunnaisia lukuja, vaan ne perustuvat matemaattisiin kaavoihin. Lisäksi muuttujat ja tietokoneen kelloa käytetään usein generointiprosessissa.

Täysin satunnaisia lukuja voidaan kuitenkin lähestyä esimerkiksi käyttämällä ulkoisia tekijöitä, kuten tietokoneen lämpötilaa tai verkkoliikennettä, generointiprosessissa.

## Katso myös
- [PHP:n virallinen rand() -dokumentaatio (englanniksi)](https://www.php.net/manual/en/function.rand.php)
- [PHP:n virallinen mt_rand() -dokumentaatio (englanniksi)](https://www.php.net/manual/en/function.mt-rand.php)
- [Satunnaislukujen generoinnin periaatteet (englanniksi)](https://medium.com/@ refurb66/how-do-computers-generate-random-numbers-4f43051107b6)