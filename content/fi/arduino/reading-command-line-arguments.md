---
title:    "Arduino: Komentoriviparametrien lukeminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

Miksi: Miksi lukea komentorivin argumentteja Arduino-ohjelmoinnissa?

Komentorivin argumenttien lukeminen on tärkeä taito, jota tarvitaan monissa Arduino-projekteissa. Tämä taito mahdollistaa käyttäjän antaa tietoja ja ohjeita ohjelmalle suoraan käyttäen komentoriviltä. Esimerkiksi käyttäjä voi syöttää tietyn lämpötila- tai valotason, johon ohjelma reagoi ja toimii sen mukaisesti. Lue lisää saadaksesi selkeän käsityksen siitä, miksi komentorivin argumenttien lukeminen on tärkeää ja miten se voidaan toteuttaa Arduino-ohjelmoinnissa.

Miten: Komentorivin argumenttien lukeminen Arduino-ohjelmassa

Komentorivin argumenttien lukeminen Arduino-ohjelmassa on helppoa ja nopeaa. Se tapahtuu sisäänrakennetun Serial-olio funktion `Serial.readString()` avulla. Tämän funktion avulla Arduino lukee komentoriviltä saadun merkkijonon ja tallentaa sen muuttujaan, jota voidaan sitten käyttää ohjelman suorituksessa. Seuraavassa esimerkissä luodaan muuttuja `input`, johon sijoitetaan komentoriviltä saatu arvo. Tämän jälkeen tulostetaan teksti käyttäen tätä arvoa.

```
Arduino ... 
// Luodaan muuttuja input;
String input;

void setup() {
  // Avataan Serial-yhteys
  Serial.begin(9600);
  // Luetaan komentorivin argumentti ja tallennetaan se muuttujaan
  input = Serial.readString();
}

void loop() {
  // Tulostetaan teksti käyttäen muuttujan arvoa
  Serial.println("Komentoriviltä saatu teksti: " + input);
}
```

Kun tämä koodi suoritetaan ja komentoriviltä annetaan esimerkiksi teksti "Testi", Arduino tulostaa seuraavan viestin:

```
Komentoriviltä saatu teksti: Testi
```

Syötteeksi voidaan antaa myös muita merkkijonoja tai numeroita, jotka voidaan sitten tallentaa ja käsitellä ohjelmassa halutulla tavalla. Tärkeää on muistaa, että komentorivin argumentit tulee antaa `Serial Monitor` -ohjelmassa, joka löytyy Arduino IDE:stä.

Syvempää tietoa komentorivin argumenttien lukemisesta

Komentorivin argumenttien lukeminen Arduino-ohjelmassa ei rajoitu pelkästään `Serial.readString()` -funktioon, vaan myös muita käyttökelpoisia funktioita on saatavilla. Esimerkiksi funktion `Serial.write()` avulla voidaan lähettää komentoriville tietoa, kun taas `Serial.parseInt()` -funktion avulla voidaan lukea ja muuntaa komentoriviltä annettuja numeroita.

Koska komentorivin argumentti on aina merkkijono, on tärkeää muistaa muuntaa se oikeaan muotoon, jos sitä halutaan käyttää numeroina tai muuna tietona ohjelmassa. Tästä syystä `Serial.parseInt()` -funktiota käytetään usein yhdessä `Serial.readString()` -funktion kanssa.

Lisäksi on tärkeää ottaa huomioon myös virheiden käsittely, jos komentorivin argumentteja käytetään esimerkiksi syöttövirheisiin. Tarkempi ja laajempi opas komentorivin argumenttien lukemiseen löytyy Arduino:n dokumentaatiosta.

Katso myös

- [Arduino Serial Communications](https://www.arduino.cc/en/Serial/Communications