---
title:                "Arduino: Satunnaisten lukujen luominen"
simple_title:         "Satunnaisten lukujen luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi?

Monet Arduino-projektit vaativat satunnaisia lukuja, kuten arpajaiset tai pelit. Satunnaisuus lisää jännitystä ja vaihtelua projekteihin. Onneksi Arduinon kirjastossa on helppokäyttöinen toiminto satunnaislukujen luomiseksi.

# Kuinka?

`Arduino -kirjasto tarjoaa alustan satunnaislukujen luomiselle. Tämä tapahtuu käyttämällä `random()` -funktiota ja määrittämällä haluttu vaihteluväli.` Arduino -funktio tuottaa kokonaislukuja (integers), mutta se voidaan muuntaa halutuksi muotoon.`

```Arduino
int satunnaisluku = random(1, 100); // luo kokonaisluvun väliltä 1-100
float satunnaisluku = random(0.0, 1.0); // luo liukuluvun väliltä 0.0-1.0
```

Tässä esimerkissä luomme satunnaisen ledin syttyneen vaihtelemaan 15-30 minuutin välein.

```Arduino
int minuutit = random(15, 30);
delay(minuutit * 60000); // muutetaan minuutit millisekunneiksi
digitalWrite(LED_PIN, HIGH); // ledi syttyy
```

# Syvemmälle

Arduino käyttää sisäistä satunnaislukugeneraattoriaan (`randomSeed()`) luodakseen satunnaislukuja.` Tämä funktio luo uuden alkuarvon (`seed`), joka perustuu laitteen sisäiseen kelloon.` Tämä varmistaa, että saamme jokaisella kerralla erilaisen satunnaisluvun.`

Voit myös määrittää `randomSeed()` -funktion manuaalisesti antamalla sille halutun alkuarvon, mikä on hyödyllistä testauksessa tai tiettyjen lukujen generoimisessa.

` Arduino tarjoaa myös muita satunnaisuutta liittyviä funktioita, kuten `randomSeed()` -funktio, joka luo jatkuvasti satunnaisia lukuja.` Näitä funktioita voi tutkia lisää Arduinon dokumentaatiosta.

# Katso myös

- [Arduino -kirjaston dokumentaatio Satunnainen](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Käyttäjäkysymys Stack Overflow -sivustolla satunnaislukujen luomisesta Arduinolla](https://stackoverflow.com/questions/12702260/how-to-generate-a-random-number-in-arduino)
- [Instructables -opas satunnaislukujen käytöstä Arduinossa](https://www.instructables.com/id/Using-Random-Numbers-With-Arduino/)