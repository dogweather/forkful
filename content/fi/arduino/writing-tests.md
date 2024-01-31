---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Testaus on koodin varmistamista suorittamalla pieniä testiohjelmia, jotka tarkistavat koodin toiminnan. Ohjelmoijat käyttävät testejä löytääkseen virheitä ja varmistuakseen, että koodi toimii suunnitellusti.

## How to:

Arduino ei tue testausta suoraan, mutta voit simuloida testausprosessia kirjoittamalla testifunktion ja kutsumalla sitä `setup()`-funktion sisällä. Tarkasta tulokset sarjaportista.

```Arduino
void setup() {
  Serial.begin(9600);
  testLedBlink();
}

void loop() {
  // Tee mitä normaalisti teet loopissa.
}

void testLedBlink() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
  
  // Tarkista, että LED vilkkuu.
  if(digitalRead(LED_BUILTIN) == LOW) {
    Serial.println("Testi onnistui!");
  } else {
    Serial.println("Testi epäonnistui.");
  }
}
```

Näet tuloksen sarjaportissa: "Testi onnistui!" tai "Testi epäonnistui."

## Deep Dive

Arduino-yhteisössä ei ole vakiintunutta testauskäytäntöä, toisin kuin ohjelmistokehityksessä yleensä. Muissa ohjelmointiympäristöissä, kuten Rubyssa ja Javascriptissä, testaus on yleistä ja hyvin tukema ominaisuus. Arduinoon voit tuoda testausominaisuuksia käyttäen esimerkiksi `aunit`-kirjastoa tai simuloimalla testikäyttäytymistä muilla kirjastoilla.

## See Also

- AUnit, Arduino unit testing library: [https://github.com/bxparks/AUnit](https://github.com/bxparks/AUnit)
