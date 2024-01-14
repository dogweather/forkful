---
title:    "Arduino: Komentoriviparametrien lukeminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi lukea komentorivin argumentteja? Tämä on hyvä kysymys, ja vastaus voi yllättää sinut. Komentorivin argumentteja voidaan käyttää kommunikointiin Arduinon ja tietokoneen välillä, mikä mahdollistaa monia hyödyllisiä toimintoja. Jatka lukemista saadaksesi lisätietoja siitä, miten tämä toimii ja miten voit hyödyntää sitä omassa projektissasi.

## Miten

Jos olet jo perehtynyt Arduinon ohjelmointiin, olet todennäköisesti käyttänyt `Serial`-kirjastoa viestintään tietokoneen kanssa. Mutta entä jos haluat lähettää pidemmän tai monimutkaisemman viestin? Tämä voi olla hankalaa käyttäen vain `Serial`-yhteyttä. Tähän tarvitaan komentorivin argumentteja.

Kun yhdistät Arduinon tietokoneeseen, voit antaa sille komentorivin argumentteja, kuten näin:

```Arduino
C:\Users\Desktop>arduino_test.exe -p COM3 -s 9600
```

Tässä esimerkissä käytetään `arduino_test.exe`-ohjelmaa, joka hyödyntää `Serial`-yhteyttä ja käyttää kahta argumenttia: porttinumeroa (`-p`) ja siirtonopeutta (`-s`). Huomaa, että näitä argumentteja käytetään ohjelman käynnistämiseen, eivätkä ne vaikuta ohjelman suorittamiseen.

Kun olet lähettänyt komentorivin argumentteja Arduinolle, voit lukea ne `Serial`-yhteyden kautta ja käyttää niitä esimerkiksi muuttujina. Tämä mahdollistaa monimutkaisemman viestinnän tietokoneen ja Arduinon välillä.

Mutta kuinka sitten kirjoitat ja luet komentorivin argumentteja itse Arduinolla? Tarvitset vain muutaman yksinkertaisen koodirivin, kuten tässä esimerkissä:

```Arduino
void setup() {
  Serial.begin(9600);  // alustetaan Serial-yhteys
}

void loop() {
  if (Serial.available()) {  // tarkistetaan, onko yhteyttä saatavilla
    String input = Serial.readString(); // luetaan vastaanotettu viesti
    Serial.print("Vastaanotit: "); // lähetetään vastaus
    Serial.println(input);
  }
}
```

Tässä koodissa ensin alustetaan `Serial`-yhteys ja sitten ohjelma tarkistaa, onko yhteys saatavilla. Jos yhteys on saatavilla, ohjelma lukee vastaanotetun viestin ja lähettää sitten vastauksen takaisin. Tämän avulla voit kirjoittaa ja lukea komentorivin argumentteja Arduinolla.

## Syvällinen sukellus

Komentorivin argumentteja voi käyttää moniin eri tarkoituksiin Arduinon ohjelmoinnissa. Esimerkiksi voit käyttää niitä asettamaan tunnistimia, aloittamaan tietyt toiminnot tai muuttamaan Arduino-ohjelmasi parametreja. Voit myös hyödyntää komentorivin argumentteja luomaan käteviä käyttöliittymiä Arduinon ja tietokoneen välillä.

On myös tärkeää huomata, että Arduinon `Serial`-yhteyttä voi käyttää vain yhteen suuntaan kerrallaan. Tämä tarkoittaa, että jos käytät komentorivin argumentteja lähettämään