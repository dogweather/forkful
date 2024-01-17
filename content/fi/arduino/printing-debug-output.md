---
title:                "Virheenkorjaustulostuksen tulostaminen"
html_title:           "Arduino: Virheenkorjaustulostuksen tulostaminen"
simple_title:         "Virheenkorjaustulostuksen tulostaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Debug-tulostus tarkoittaa ohjelmien virheiden etsimistä ja korjaamista koodin tulostamalla. Ohjelmoijat tekevät sitä varmistaakseen, että koodi toimii oikein ja löytää mahdolliset virheet.

 ## Miten:

Debng-tulostuksen käyttö Arduino-ohjelmoinnissa on helppoa. Voit tulostaa haluamasi tiedot sarjaportin kautta valitsemalla oikean avainsanan. Tässä muutama esimerkki, joissa käytämme avainsanaa ```Serial ... ```:
 
```Arduino
 Serial.begin(9600); // alustaa sarjaportin baudinopeudella 9600

 Serial.print("Hei maailma!"); // tulostaa vakiotekstin

 int numero = 10;
 Serial.println(numero); // tulostaa numeron 10 omalle rivilleen
```

Näet tulostuksen avulla sarjaportin monitorissa (Monitor).

## Syvemmälle:

Debug-tulostus on ollut tärkeä osa ohjelmoinnin historiaa jo pitkään. Aikaisemmin koodin tulostaminen tapahtui papereille tai käyttöliittymään, mutta nyt se voidaan tehdä yksinkertaisesti sarjaportin avulla. On myös muita tapoja toteuttaa debug-tulostus, kuten käyttämällä LED-valoja (valot vilkkuvat koodin suorituksen aikana) tai näytönohjaimia (näyttää tietoja LCD-näytöllä).

Käytetyn avainsanan valinta riippuu siitä, mitä haluat tulostaa ja kuinka haluat sen näkyvän. Tässä muutamia vaihtoehtoja:

- ```Serial.print```: Tulostaa koodin suorituksessa muuttuvia arvoja ja merkkejä.
- ```Serial.println```: Tehnyt samaa kuin ```Serial.print``` mutta lisää loppuun rivinvaihto.
- ```Serial.write```: Antaa sinulle mahdollisuuden tulostaa binäärimuodossa olevia tietoja.
- ```Serial.flush```: Tyhjentää sarjaportin tulostopuskurin ennen komennon suorittamista.

Voit muokata sarjaportin asetuksia, kuten baudinopeutta ja puskurin kokoa, tarpeidesi mukaan. Voit myös käyttää muita Arduino-kirjastoja saadaksesi lisäominaisuuksia, kuten värinän ja äänen tuottamisen.

## Katso myös:

Jos haluat oppia lisää debug-tulostuksesta Arduino-ohjelmoinnissa, tutustu näihin lähteisiin:

- Arduino-ohjeet: Sarjaportin käyttö - https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Ohjelmointiopas: Tulostaminen sarjaportin avulla - https://programmingguide.net/serial-print-function-arduino