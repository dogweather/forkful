---
title:    "Arduino: Tekstitiedoston lukeminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

On monia hyödyllisiä ja mielenkiintoisia tapoja käyttää Arduino-mikrokontrolleria, ja yksi niistä on tiedostojen lukeminen. Tässä blogikirjoituksessa kerromme, miksi Arduino-ohjelmoijan kannattaa oppia lukemaan tiedostoja ja miten se tehdään.

## Miten

Tiedoston lukeminen Arduinossa on melko yksinkertaista. Se vaatii vain muutaman rivin koodia, ja voit käyttää samaa koodia lukemaan erilaisia tiedostomuotoja, kuten txt, csv ja xml. Alla on esimerkki koodista, joka lukee tekstitiedoston ja tulostaa sen sisällön sarjamonitorille:

```Arduino
File tiedosto = SD.open("tiedosto.txt"); // Avaa tiedosto
while (tiedosto.available()) { // Käy läpi tiedosto niin kauan kuin siinä on sisältöä
    Serial.println(tiedosto.read()); // Tulosta tiedoston sisältö seriaportille
}
tiedosto.close(); // Sulje tiedosto
```

Tämä koodi käyttää SD-korttikirjastoa, joten sinun täytyy ensin asentaa se Arduino IDE:hen. Voit muuttaa koodia tarpeen mukaan, esimerkiksi voit tallentaa tiedoston sisällön muuttujaan ja käsitellä sitä edelleen.

## Syvemmälle

Tiedostojen lukeminen Arduinossa avaa mahdollisuuksia monille eri projekteille. Voit esimerkiksi tallentaa järjestelmän tilaa tai kerätä tietoja erilaisista antureista ja tallentaa ne tiedostoon myöhempää käyttöä varten. Voit myös käyttää tiedostoja laajentamaan laitteesi toimintoja tarvitsematta muuttaa koodia.

Kannattaa myös huomata, että Arduinolla on rajoitettu tallennustila, joten sinun kannattaa olla tarkkana tiedostojen koon kanssa ja poistaa tarpeettomat tiedostot säännöllisesti.

## Katso myös

Tässä blogikirjoituksessa käytiin läpi vain yksi tapa lukea tiedostoja Arduinossa. Voit tutkia lisää tiedostojen käsittelyyn liittyviä toimintoja SD-korttikirjastosta ja etsiä muita tapoja lähestyä tiedostojen lukemista. Löydät myös paljon hyödyllisiä resursseja ja esimerkkikoodeja Arduino-yhteisöltä ja foorumeilta.

Lopuksi, muista olla varovainen tiedostojen kanssa ja varmista, että ymmärrät mitä koodisi tekee ennen kuin tallennat tiedostoja. Onnea koodaukseen!