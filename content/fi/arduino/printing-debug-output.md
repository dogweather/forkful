---
title:                "Arduino: Virheenjäljitystulostuksen tulostaminen"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Onko Arduino-ohjelmoinnissa turhauttavaa, kun et saa selville miksi koodi ei toimi? Tulostaminen debug-tietoa voi auttaa sinua löytämään ongelmia ja korjaamaan ne nopeammin.

## Miten tehdä
Arduino-koodissa, voit käyttää `Serial.println()` -funktiota tulostaa debug-tietoa. Esimerkki:

```Arduino
void setup() {
  pinMode(13, OUTPUT);
  Serial.begin(9600);
}

void loop() {
  digitalWrite(13, HIGH);
  Serial.println("LED vilkkuu nopeasti");
  delay(1000);
  digitalWrite(13, LOW);
  Serial.println("LED ei vilku");
  delay(1000);
}
```
Output:

```
LED vilkkuu nopeasti
LED ei vilku
```

Voit myös käyttää `Serial.print()` ja `Serial.write()` -funktioita tulostamaan muuttujia ja merkkijonoja. Esimerkki:

```Arduino
int luku = 10;
Serial.print("Luku: ");
Serial.println(luku);
```
Output:

```
Luku: 10
```

## Syvemmälle
Voit käyttää myös `Serial.begin()` -funktiota määrittämään oikea baud rate debug-tulostukseen, mikä voi auttaa nopeuttamaan tiedonsiirtoa. Voit myös lisätä `Serial.flush()` -funktion tulostamaan kaiken odottavan datan ennen kuin jatkat koodia.

## Katso myös
- [Serial println() reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Debugging in Arduino with Serial Monitor](https://create.arduino.cc/projecthub/Arduino_Genuino/debugging-arduino-with-serial-monitor-84b113?ref=tag&ref_id=debugging&offset=0)
- [Print debugging in Arduino](https://medium.com/@plipr/how-i-survived-to-my-first-project-with-arduino-part-3-print-debugging-d67b1e6ddbd7)