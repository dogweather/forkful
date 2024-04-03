---
date: 2024-01-20 17:57:10.440034-07:00
description: "Tekstin etsiminen ja korvaaminen tarkoittaa sopivan merkkijonon l\xF6\
  yt\xE4mist\xE4 tekstist\xE4 ja sen muuttamista toiseksi. Ohjelmoijat k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 toimintoa\u2026"
lastmod: '2024-03-13T22:44:56.810414-06:00'
model: gpt-4-1106-preview
summary: "Tekstin etsiminen ja korvaaminen tarkoittaa sopivan merkkijonon l\xF6yt\xE4\
  mist\xE4 tekstist\xE4 ja sen muuttamista toiseksi."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## Kuinka:
```Arduino
String originalText = "Hello, this is an example.";
String searchText = "example";
String replaceText = "sample";
String resultText;

void setup() {
  Serial.begin(9600);
  
  if (originalText.indexOf(searchText) >= 0) {
    resultText = originalText.substring(0, originalText.indexOf(searchText));
    resultText += replaceText;
    resultText += originalText.substring(originalText.indexOf(searchText) + searchText.length());
  } else {
    resultText = originalText;
  }

  Serial.println("Original Text: " + originalText);
  Serial.println("After Replace: " + resultText);
}

void loop() {
  // Nothing to do here
}
```
Tuloste:
```
Original Text: Hello, this is an example.
After Replace: Hello, this is an sample.
```

## Syväsukellus:
Tekstin korvaaminen oli keskeistä varhaisissa tietokoneissa tekstipohjaisten käyttöliittymien ja ohjelmoinnin aikana. `String`-objektin metodit `indexOf()` ja `substring()` ovat perusvälineitä tekstinkäsittelyyn Arduinossa. Vaikka nämä toiminnot ovat hyödyllisiä, ne kuluttavat myös muistia, mikä on tärkeää huomioida resurssirajoitteisissa järjestelmissä kuten Arduinossa. Eri kirjastot ja kielet tarjoavat erilaisia ja tehokkaampia ratkaisuja, mutta simplicitetti voittaa tietyissä tilanteissa.

## Katso Myös:
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ std::string-kirjaston käyttö Arduinossa: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- RegEx-kirjastot tekstinkäsittelyyn monimutkaisempiin etsintä- ja korvaustarpeisiin.
