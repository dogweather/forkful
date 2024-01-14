---
title:                "Arduino: Tulostus virheenkorjauskäyttöliittymän avulla"
simple_title:         "Tulostus virheenkorjauskäyttöliittymän avulla"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

### Miksi

Miksi tulostaa virheenkorjaustulosteita Arduinon ohjelmoinnissa? Debug-tulosteet ovat tärkeitä ohjelman toiminnan ymmärtämiseksi ja ongelmien selvittämiseksi. Ne auttavat ohjelmoijaa havaitsemaan virheitä ja korjaamaan niitä nopeasti, jolloin ohjelman kehittäminen ja testaaminen on helpompaa.

### Kuinka

Debug-tulosteiden tulostamiseen Arduino-ohjelmassa voi käyttää Serial.println()-funktiota, joka tulostaa halutun viestin sarjaportin kautta. Tämä voidaan tehdä esimerkiksi seuraavalla koodilla:

```
Arduino.setup(){
    Serial.begin(9600); // alustetaan sarjaportti
}

void loop(){
    Serial.println("Ohjelma käynnistyi."); // tulostetaan viesti sarjaporttiin
    // muu koodi
}
```

Tämä koodi tulostaa tekstin "Ohjelma käynnistyi." sarjaporttiin ja se voidaan lukea esimerkiksi Arduino IDE:n sarjaportti-ikkunasta. Debug-tulosteiden avulla voidaan myös näyttää muuttujien arvoja, kuten esimerkiksi:

```
int myNumber = 25; // määritetään muuttuja

void loop(){
    Serial.print("Muuttujan arvo on: "); // tulostetaan ensin merkkijono
    Serial.println(myNumber); // sitten muuttujan arvo
    // muu koodi
}
```

Tämä koodi tulostaisi sarjaporttiin tekstin "Muuttujan arvo on: 25". Näin debug-tulosteiden avulla voidaan tarkastella ohjelman sisäistä tilaa ja havaita mahdollisia virheitä.

### Syvempää tietoa

Debug-tulosteiden käyttö ei rajoitu vain sarjaporttiin tulostamiseen. Niitä voidaan myös lähettää esimerkiksi Bluetooth- tai Wi-Fi-moduulin kautta, jolloin tulosteet voidaan lukea esimerkiksi älypuhelinsovelluksesta. Lisäksi tulosteisiin voidaan lisätä erilaisia merkkijonoja ja muuttujia käyttämällä erilaisia tulostofunktioita, kuten esimerkiksi Serial.printf(), joka hyödyntää C-kielen vakiotoimintoja.

On myös tärkeää huomata, että debug-tulosteet voivat hidastaa ohjelman suorituskykyä, joten ne kannattaa poistaa lopullisesta koodista. Tähän voidaan käyttää esimerkiksi ehtolauseita, jotka tarkastavat, onko ohjelma käynnissä kehitys- vai lopullisessa käyttötarkoituksessa.

### Katso myös

- [Arduino Serial.println()-funktio](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino Serial.printf() -funktio](https://www.arduino.cc/reference/en/language/functions/communication/serial/printf/)
- [Arduino-kehittäjien yhteisö](https://forum.arduino.cc/)