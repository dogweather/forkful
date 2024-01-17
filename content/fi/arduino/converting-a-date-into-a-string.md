---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Arduino: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämäärän esittämistä tekstilukuna. Tätä tehdään yleisesti ohjelmointikohteissa, joissa päivämäärää tarvitaan eri muodoissa tai esimerkiksi tallennettavaksi tiedostoksi.

# Miten:
Arduino on avoin ja joustava alusta, johon voit ohjelmoida päivämäärän muuntamisen merkkijonoksi helposti. Esimerkkikoodi alla olevassa esimerkissä muuntaa tämän päivän päivämäärän merkkijonoksi. Tulostus näkyy sarjaportissa, kun Arduinon kytketään tietokoneeseen.

```
Arduino
String today = String(day()) + "/" + String(month()) + "/" + String(year()); 
Serial.println(today);
```
Tulostus voi olla esimerkiksi "28/4/2020".

# Syväsukellus:
Päivämäärän muuntaminen merkkijonoksi on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Aikaisemmin se suoritettiin monimutkaisilla laskutoimituksilla, mutta nykyään se on helpompaa ja nopeampaa erilaisten ohjelmointialustojen ansiosta.

On myös muita tapoja muuttaa päivämäärä merkkijonoksi, kuten käyttämällä kirjastoja tai omia funktioita. Kuten aina ohjelmoinnissa, kannattaa etsiä ja kokeilla erilaisia ratkaisuja ja valita itselleen sopivin vaihtoehto.

# Katso myös:
- [Arduino String reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Converting a date into a string](https://www.geeksforgeeks.org/converting-date-string-using-arduino/)
- [Arduino Date and Time functions](https://www.arduino.cc/en/Tutorial/BuiltInExamples/DateTime)