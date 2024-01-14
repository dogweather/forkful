---
title:    "Arduino: Tulostamassa vianmääritystietoja"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi tulostaisit debuggaustietoa Arduino-ohjelmoinnissasi? Debuggaustieto auttaa sinua ymmärtämään koodissasi tapahtuvia asioita, kuten virheitä ja muuttujien arvoja. Se on hyödyllistä etenkin kun yrität ratkaista ongelmia tai kehittää koodiasi.

## Miten

Alla on esimerkki koodista, jossa tulostetaan debuggaustietoa Arduino IDE:n sarjaportti-työkalun avulla:

```Arduino
int ledPin = 13;
int buttonPin = 2;
int buttonState = 0;

void setup() {
  pinMode(ledPin, OUTPUT);
  pinMode(buttonPin, INPUT);
  Serial.begin(9600);
}

void loop() {
  buttonState = digitalRead(buttonPin); // lue napin tila
  digitalWrite(ledPin, buttonState); // aseta LEDin tila napin tilan mukaan
  Serial.println("Napin tila: " + String(buttonState)); // tulosta debuggaustieto sarjaporttiin
  delay(100); // odota 100 millisekuntia
}
```

Kun käytät tätä koodia, voit avata sarjaportti-työkalun arvojen tarkastelemiseksi ja vahvistamiseksi. Jos nappi on painettuna, sarjaportista tulostuu "Napin tila: 1", ja jos nappi on irrotettuna, tulostuu "Napin tila: 0". Tämä auttaa sinua seuraamaan napin tilan muutoksia ja varmistamaan, että koodi toimii odotetusti.

## Syvempää tietoa

Debuggaustietoa ei kannata tulostella liikaa, sillä se hidastaa koodin suoritusta. Sen sijaan voit asettaa tiettyjä ehtoja, jolloin debuggaustieto tulostetaan vain silloin kun tarpeen. Esimerkiksi voit käyttää if-lausetta tietyn virheen tarkastamiseen ja tulostaa virheviestin vain silloin kun kyseinen virhe tapahtuu.

Tärkeintä on käyttää debuggaustietoa fiksusti. Se auttaa sinua löytämään ongelmia ja optimoimaan koodiasi, mutta liiallinen tulostelu voi hidastaa ohjelmasi suorituskykyä. Varmista siis, että poistat turhat debuggaustulosteet ennen kuin kirjoitat koodisi tuotantokäyttöön.

## Katso myös

- [Arduino Reference](https://www.arduino.cc/reference/en/): Officiaali Arduino referenssisivusto, jossa löydät tietoa sarjaportin käytöstä ja muista hyödyllisistä toiminnoista.
- [Programming Electronics Academy](https://programmingelectronics.com/): Sivusto, jossa on paljon opetusmateriaalia Arduino-ohjelmoinnista ja elektroniikasta. Heillä on myös YouTube-kanava, joka tarjoaa hyödyllisiä opetusvideoita.
- [Arduino Community Forum](https://forum.arduino.cc/): Täällä voit kysyä kysymyksiä ja saada apua muilta Arduino-harrastajilta.