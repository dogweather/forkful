---
title:    "Arduino: Sattumanvaraisten numeroiden luominen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Usein Arduino-projekteissa on tarve käyttää satunnaislukuja, kuten arpomalla numeroita pelissä tai valitsemalla satunnainen LED-väri. Tämä tekee projekteista monipuolisempia ja mielenkiintoisempia.

## Kuinka tehdä

Arduino tarjoaa valmiin randomSeed()-funktion, joka käyttää laitteen sisäisten komponenttien ajoittaisuutta satunnaislukujen generoimiseksi. Voit myös tehostaa satunnaislukujen generointia käyttämällä esimerkiksi analogWrite() tai digitalRead() komentoja osana algoritmia:

```Arduino
void setup(){
  randomSeed(analogRead(A0));
  //tämä käyttää A0-pinnin lukemia satunnaislukujen pohjana
}

void loop(){
  int randomNumber = random(10,100); //generoi satunnaisluvun väliltä 10-100
  digitalWrite(ledPin, HIGH);
  delay(randomNumber); //odottaa satunnaisen määrän millisekunteja
  digitalWrite(ledPin, LOW);
}
```

Esimerkkilähdön avulla voit nähdä, kuinka LEDin välkkyminen muuttuu satunnaisemmaksi:

```
LED vilkkuu nopeasti.
Odota 76 millisekuntia.
LED vilkkuu nopeasti.
Odota 14 millisekuntia.
LED vilkkuu hitaammin.
Odota 54 millisekuntia.
LED vilkkuu nopeasti.
Odota 91 millisekuntia.
```

## Syvemmälle

Vaikka randomSeed()-funktio käyttääkin laitteen sisäisiä komponentteja, se ei silti aina generoi täysin satunnaisia lukuja. Tämä johtuu siitä, että esimerkiksi analogisten pinnien jännitetasot voivat vaihdella hyvinkin pienessä mittakaavassa. Tämä voi johtaa siihen, että satunnaislukujen sarjasta saattaakin löytyä jonkinlainen malli. Siksi onkin hyvä käyttää muita lisäkeinoja, kuten esimerkiksi ulkoista kohinaa tai antureita satunnaislukujen luomiseen.

Kun käytät Arduinoa projekteissasi, voit myös hyödyntää sen kirjastoja, jotka ovat erikoistuneet satunnaislukujen generoimiseen. Esimerkiksi RandomSeed-rinnakkaiskirjasto tarjoaa enemmän erilaisia algoritmeja satunnaislukujen luomiseen.

## Katso myös

- Arduino Reference: [randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- Arduino Playground: [RandomSeed Library](https://playground.arduino.cc/Code/RandomSeed/)
- RandomSeed Library: [GitHub](https://github.com/vizembled/RandomSeed)