---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- fi/arduino/searching-and-replacing-text.md
date:                  2024-01-20T17:57:10.440034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Tekstin etsiminen ja korvaaminen tarkoittaa sopivan merkkijonon löytämistä tekstistä ja sen muuttamista toiseksi. Ohjelmoijat käyttävät tätä toimintoa automatisoidakseen ja yksinkertaistaakseen suuria datamääriä tai konfiguraatioiden muokkausta.

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
