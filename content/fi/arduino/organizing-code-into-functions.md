---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:09:15.744244-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa sen hajottamista uudelleenkäytettäviin osiin, joista jokainen hoitaa tietyn tehtävän. Ohjelmoijat tekevät niin, jotta koodista tulee helpompi lukea, virheenkorjata ja uudelleenkäyttää. Se on kuin Lego-palikoiden laittamista lokeroihin - se säästää sinut penkomasta sekavaa kasaasi joka kerta, kun haluat rakentaa jotain.

## Kuinka:
Kuvittele haluavasi saada LEDin vilkkumaan. Ilman funktioita `loop`-toimintosi on sekava sotku. Funktioiden kanssa se on siisti. Tältä se näyttää:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Vilkuttaa LEDiä joka 500ms
}

// Funktio LEDin vilkuttamiseen
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Esimerkkitulos: LEDisi vilkkuu tyytyväisenä, ja koodin tarkoitus on selvä yhdellä silmäyksellä.

## Syväsukellus
Ennen funktion käyttöä ohjelmointi oli lineaarinen automatka; näit jokaisen kuopan alusta loppuun. Funktioiden jälkeen se on enemmän kuin lentojen hyppeleminen - voit siirtyä tärkeisiin osiin. Historiallisesti aliohjelmat (alkuperäiset funktiot) olivat vallankumous ohjelmoinnissa, antaen koodareille mahdollisuuden välttää toiston – se on DRY-periaatetta, Älä Toista Itseäsi. Vaihtoehdot funktioille voivat sisältää makroja tai luokkien käyttöä oliopohjaisessa ohjelmoinnissa (OOP). Yksityiskohtaisesti? Kun määrittelet funktion, annat kääntäjälle sinisen printin tehtävän suorittamiseksi. Arduino-ohjelmoinnissa määrittelet usein void-funktioita, jotka toimivat yksinkertaisina komentoina mikrokontrollerille, mutta funktiot voivat myös palauttaa arvoja, mikä tekee niistä monipuolisempia.

## Katso Myös
Lisätietoja funktioista, tutustu näihin:

- Arduino'n virallinen funktioreferenssi: https://www.arduino.cc/reference/en/language/functions/
- Lisätietoja DRY-periaatteesta: https://fi.wikipedia.org/wiki/Älä_toista_itseäsi
- Kertaus aliohjelmien historiasta: https://en.wikipedia.org/wiki/Subroutine
