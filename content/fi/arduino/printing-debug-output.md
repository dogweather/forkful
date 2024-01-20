---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tulostaminen 'debug output' on työkalu, jonka avulla ohjelmoijat voivat nähdä ohjelmiensa toiminnan askel askeleelta. Tämä auttaa tunnistamaan ja korjaamaan virheitä sekä ymmärtämään paremmin ohjelman toimintaa.

## Näin se tehdään:
Voit tulostaa debug-tietoja Arduino-iskäntäohjelmassa hyvin yksinkertaisesti. Lähetä vain viestejä sarjaportin kautta avoimeen valvontaikkunaan. Tässä on esimerkki koodipätkä, joka lähettää rivin tekstiä "Hello, World!".

```Arduino
void setup() {
  Serial.begin(9600);   
}

void loop() {
  Serial.println("Hello, World!"); 
  delay(1000);
}
```

Tässä koodissa `Serial.begin(9600);` asettaa tiedonsiirtonopeuden ja `Serial.println("Hello, World!");` lähettää rivin tekstiä sarjamonitorille. `delay(1000);` pysäyttää ohjelman sekunniksi, ennen kuin lähetetään seuraava viesti. 

## Syvä sukellus
Alun perin tulostaminen debug-tietoihin kehitettiin ohjelmoijien työkaluksi koodin ongelmien tunnistamiseksi ja ratkaisemiseksi, kuten ohjelmavirheiden tai toiminnallisien ongelmien etsiminen. Arduinolla tämä toteutettiin SPP:llä (Serial Port Protocol), joka on yksinkertainen ja tehokas tapa kommunikoida virheraportit.

Vaihtoehtoisia menetelmiä ovat mm. LCD-näytöt, tiedonsiirto internetin tai paikallisen verkon kautta tai tiedon tallentaminen SD-muistikortille. Näiden avulla voi olla käytännöllistä tarkastella tietoja reaaliajassa tai jopa paikan päällä, mutta ne vaativat lisäkomponentteja tai -kirjastoja.

Iteohjelmointiympäristössä, Arduinon `Serial`-olio sisältää funktion `println`, joka hoitaa itse tekstirivin lähettämisen ja sisäänrakennetun rivinvaihdon. Serial-luokka käyttää RXTX-kirjastoa (Receive/Transmit), joka on laajasti käytetty sarjaliikennettä tarvitsevissa Java-sovelluksissa.

## Katso myös
Enemmän tietoa debuggauksesta ja sen työkaluista löydät näistä lähteistä:

1. [Arduino Debugging](https://www.arduino.cc/en/Tutorial/LibraryExamples/SoftwareSerialExample)
2. [Advanced Arduino Debugging](https://learn.sparkfun.com/tutorials/advanced-arduino-debugging)
3. [Understanding Arduino UNO Hardware Design](https://www.allaboutcircuits.com/technical-articles/understanding-arduino-uno-hardware-design/)
4. [Arduino Serial Communications Guide](https://store.arduino.cc/usa/arduino-hackspace-guide)