---
title:    "Arduino: Satunnaislukujen generointi."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi: Satunnaislukujen generointi Arduino-ohjelmoinnissa

Satunnaislukujen generointi on tärkeä osa monia Arduino-projekteja, sillä se mahdollistaa erilaisten langattomien sensorien ja aktuaattorien ohjelmoimisen. Lisäksi satunnaislukujen generointi voi tuoda lisää mielenkiintoa ohjelmointiin ja mahdollistaa esimerkiksi pelien luomisen.

## Kuinka: Esimerkkejä koodista ja tulosteista

Satunnaislukujen generoiminen Arduino-ohjelmoinnissa on helppoa, kun käytetään "random()" -funktiota. Tässä esimerkissä generoidaan satunnainen kokonaisluku välillä 0-99 ja tulostetaan se sarjamonitoriin:

```Arduino
int random_luku = random(0, 100); // generoi satunnaisen kokonaisluvun välillä 0-99
Serial.println(random_luku); // tulostaa satunnaisluvun sarjamonitoriin
```

Tässä esimerkissä käytetään "randomSeed()" -funktiota, joka asettaa satunnaislukugeneraattorin alkutilaan halutulla luvulla (esim. analogisen anturin lukema). Näin saadaan erilainen satunnaislukujen sarja joka kerta ohjelmaa suoritettaessa:

```Arduino
randomSeed(analogRead(A0)); // asettaa satunnaislukugeneraattorin alkutilaan analogisen anturin lukemalla
int random_luku = random(0, 100); // generoi satunnaisen kokonaisluvun välillä 0-99
Serial.println(random_luku); // tulostaa satunnaisluvun sarjamonitoriin
```

## Syväsukellus: Lisätietoa satunnaislukujen generoinnista

Satunnaislukujen generointi perustuu ohjelmassa olevaan satunnaislukugeneraattoriin, joka tuottaa arvoja ennalta määrättyihin välille. Arduino-kirjastossa käytetään taustalla "pseudorandom" -tekniikkaa, joka ei generoi täysin satunnaisia lukuja, vaan perustuu matemaattisiin kaavoihin. Tämä johtaa siihen, että samalla "satunnaislukugeneraattorilla" voi saada samanlaisen satunnaislukujen sarjan samanlaisissa olosuhteissa. Tästä syystä onkin tärkeää käyttää "randomSeed()" -funktiota, jotta saadaan erilaisia satunnaislukujen sarjoja.

## Katso myös

- https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- https://www.tutorialspoint.com/compile_arduino_online.php