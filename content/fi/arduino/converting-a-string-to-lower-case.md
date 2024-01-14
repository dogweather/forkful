---
title:    "Arduino: Merkkijonon muuttaminen pienaakkosiksi"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Joskus Arduino-ohjelmoinnissa tarvitaan muuttaa käyttäjän syöttämästä merkkijonosta kaikki kirjaimet pieniksi kirjaimiksi. Tähän voi esimerkiksi olla tarvetta, jos halutaan vertailla käyttäjän syöttämää merkkijonoa johonkin tiettyyn arvoon ja on helpompi käsitellä kaikki pienillä kirjaimilla varustettuja merkkijonoja.

## Miten tehdä se

Käytännöllisesti katsottuna merkkijonon muuttaminen pieniksi kirjaimiksi Arduino-ohjelmalla on varsin yksinkertaista. Seuraava esimerkki näyttää, miten se tehdään:

```Arduino
String syote = "HeLLo WorlD";
syote.toLowerCase();
Serial.println(syote);
```

Tämä esimerkki aluksi luo merkkijonon "HeLLo WorlD" ja tallentaa sen muuttujaan "syote". Sitten käytetään "toLowerCase()" -metodia muuttujan arvon muuttamiseksi pieniksi kirjaimiksi. Lopuksi käyttämällä "Serial.println(syote)", tulostuu Sarjaporttiin "hello world".

## Syvällisempi sukellus

Arduinoa ohjelmoidessa on tärkeää muistaa, että se käyttää C++ -kieltä, joten kaikki C++ -kielen metodit ovat käytettävissä myös Arduino-koodissa. Tästä syystä Arduino tukee myös "toLowerCase()" -metodia pienien kirjainten muuttamiseksi.

On myös syytä huomata, että merkkijonon muuttaminen pieniksi kirjaimiksi ei muuta merkkijonon sisältöä, vaan luo uuden muuttujan, jolla on pienet kirjaimet. Alkuperäinen muuttuja muuttumattomana.

## Katso myös

- [Arduino Programming Language Reference](https://www.arduino.cc/reference/en/)
- [C++ String toLowerCase() -metodin dokumentaatio](https://www.cplusplus.com/reference/string/string/tolower/)

Kiitos lukemisesta ja onnea Arduino-ohjelmoinnin kanssa! 🚀