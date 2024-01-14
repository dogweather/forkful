---
title:                "Arduino: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi: Miksi kirjoittaa tekstitiedosto?

Tekstiedostojen kirjoittaminen on tärkeä osa ohjelmoinnin maailmaa, sillä se on tapa tallentaa ja jakaa tietoa. Arduino-ympäristössä tekstitiedostojen kirjoittaminen on erityisen hyödyllistä esimerkiksi datan tallentamisessa ja jakamisessa eri laitteiden välillä.

## Kuinka: Tekstitiedoston kirjoittaminen Arduino-ympäristössä

Arduino-ympäristön `Serial`-kirjastolla voidaan kirjoittaa tekstitiedostoja esimerkiksi sarjaväylän kautta. Alle on esimerkki koodista, joka kirjoittaa tekstitiedoston ja lähettää sen sarjaväylän kautta:

```Arduino
// Alustetaan sarjaväylä
Serial.begin(9600);

// Määritellään tiedoston nimi ja muuttuja, johon tallennetaan tiedoston sisältö
String tiedostonimi = "data.txt";
String sisalto = "Tämä on tekstitiedoston sisältö";

// Aloitetaan tiedoston kirjoitus
File tiedosto = SD.open(tiedostonimi, FILE_WRITE);

// Tarkistetaan, että tiedoston kirjoitus onnistui
if (tiedosto) {
  // Kirjoitetaan teksti tiedostoon
  tiedosto.println(sisalto);
  // Suljetaan tiedosto
  tiedosto.close();
  
  // Lähetetään tiedoston sisältö sarjaväylän kautta
  Serial.println(sisalto);
} else {
  // Jos tiedostoa ei pystytty avaamaan, lähetetään virheilmoitus sarjaväylän kautta
  Serial.println("Virhe avattaessa tiedostoa!");
}
```

Kun koodi suoritetaan, tiedosto `data.txt` luodaan ja siihen kirjoitetaan teksti `Tämä on tekstitiedoston sisältö`. Lisäksi tiedoston sisältö lähetetään myös sarjaväylän kautta.

## Syvemmälle: Lisätietoa tekstitiedoston kirjoittamisesta

Tekstitiedoston kirjoittaminen on tärkeä osa datan tallentamista ja jakamista eri laitteiden välillä. Arduino-ympäristössä `Serial`-kirjastolla voidaan lähettää tekstiä ja dataa sarjaväylän kautta, mutta tiedostojen tallentaminen ja lukeminen `SD`-kirjaston avulla antaa enemmän mahdollisuuksia datan käsittelyyn ja tallentamiseen.

Tekstitiedostojen lisäksi Arduino-ympäristössä voidaan käsitellä myös muita tiedostomuotoja, kuten `.csv`-tiedostoja, joissa data on järjestetty taulukkomuotoon pilkulla eroteltuna. Tiedostojen käsittely Arduino-ympäristössä vaatii kuitenkin tarkkaavaisuutta ja huolellisuutta, sillä väärin formatoidut tai virheelliset tiedostot voivat aiheuttaa ongelmia.

## Katso myös

- [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Reference - SD](https://www.arduino.cc/reference/en/libraries/sd/)
- [W3Schools - .csv](https://www.w3schools.com/whatis/whatis_csv.asp)