---
date: 2024-01-27 20:32:38.558949-07:00
description: "Miten: Arduino tarjoaa yksinkertaisia funktioita satunnaislukujen tuottamiseen:\
  \ `randomSeed()` ja `random()`. Aloita alustamalla satunnaislukugeneraattori\u2026"
lastmod: '2024-03-13T22:44:56.821035-06:00'
model: gpt-4-0125-preview
summary: Arduino tarjoaa yksinkertaisia funktioita satunnaislukujen tuottamiseen.
title: Satunnaislukujen generointi
weight: 12
---

## Miten:
Arduino tarjoaa yksinkertaisia funktioita satunnaislukujen tuottamiseen: `randomSeed()` ja `random()`. Aloita alustamalla satunnaislukugeneraattori varmistaaksesi, että saat eri numerosarjan joka kerta kun ohjelmasi suoritetaan. Usein käytetty lähestymistapa on alustaa lukema analogisen lukemalla kytkemättömästä pinnistä.

```Arduino
void setup() {
  Serial.begin(9600);
  // Alusta satunnaisluku
  randomSeed(analogRead(0));
}

void loop() {
  // Luo satunnaisluku väliltä 0 ja 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Viivästys sekunnin ajan luettavuuden vuoksi
}
```

Yllä oleva ohjelma alustaa satunnaislukugeneraattorin `setup()`-funktiossa ja tuottaa uuden luvun välillä 0 ja 99 jokaisella silmukan iteraatiolla, tulostaen luvun sarjamonitoriin.

Esimerkkituloste:
```
42
17
93
...
```

## Syväsukellus
Arduinon `random()`-funktio hyödyntää pinnan alla pseudo-satunnaislukugeneraattoria (PRNG), joka noudattaa determinististä sekvenssiä, mutta näyttää tilastollisesti satunnaiselta. Sekvenssin alkuperäinen arvo, eli siemen, vaikuttaa suuresti sen ennakoimattomuuteen, siksi `randomSeed()`-funktion yleinen käyttö jokseenkin satunnaisella syötteellä on hyvä lähtökohta. On tärkeää huomata, että Arduinon tuottama satunnaisuus on riittävä useimmille harrastajaprojekteille, mutta se ei ehkä täytä korkean turvallisuuden sovellusten kriteereitä ajan mittaan ennustettavuutensa vuoksi. Salaustarkoituksiin on suositeltavaa tutustua monimutkaisempiin algoritmeihin ja laitteiston satunnaislukugeneraattoreihin (HRNG), jotka voivat tarjota todellista satunnaisuutta hyödyntämällä fyysisiä prosesseja.
