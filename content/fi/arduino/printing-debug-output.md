---
title:                "Vianjäljitystulostus"
html_title:           "Arduino: Vianjäljitystulostus"
simple_title:         "Vianjäljitystulostus"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Miksi tulostetaan vianmääritystietoja? Yksinkertaisesti sanottuna, kun ohjelmoimme, haluamme varmistaa, että koodimme toimii oikein ja että seuraamme sen toimintaa. Tulostamalla vianmääritystietoja voimme helposti seurata, mitä koodi tekee ja etsiä mahdollisia virheitä.

## Miten
Tässä esimerkissä käytämme ```Serial.print()``` -toimintoa tulostamaan vianmääritystietoja Arduino IDEen sarjaporttikonsoliin:

```
void setup() {
  Serial.begin(9600); // alustetaan sarjaportti 9600 baudin nopeudella
}

void loop() {
  // luodaan muuttujia
  int x = 10;
  float y = 3.14;
  String z = "Terve maailma";

  // tulostetaan muuttujat sarjaporttiin
  Serial.print("x: ");
  Serial.println(x);
  Serial.print("y: ");
  Serial.println(y);
  Serial.print("z: ");
  Serial.println(z);

  // odotetaan 1 sekunti ennen seuraavaa kierrosta
  delay(1000);
}
```

Tämä koodiesimerkki tulostaa arvoja muuttujista sarjaporttiin jokaisen ```loop()```-toiminnon suorituksen jälkeen. Voit avata sarjaporttikonsolin valitsemalla "Työkalut" -valikosta "Sarjaportti" tai painamalla ```Ctrl + Shift + M```.

**Tärkeä huomio**: jos et avaa sarjaporttikonsolia tai et ole yhdistänyt Arduinota tietokoneeseen, et näe mitään tulostusta.

## Syventyvä sukellus
Tulostaminen sarjaporttiin voi olla erittäin hyödyllistä vianmäärityksessä. Voit käyttää erilaisia ```Serial``` -toimintoja, kuten ```print()```, ```println()```, ```write()``` ja ```print()``, riippuen siitä, mitä tietoa haluat tulostaa. Voit myös käyttää erilaisia muotoiluja, kuten ```Serial.print("x: ");```ja ```Serial.println(y, 2);```jne.

Lisäksi voit käyttää myös ehtolauseita tai koodin suoritusajan seurantaa ja tulostaa sen sarjaporttiin. Näin voit tarkistaa, ovatko tiettyjen osien koodistasi hitaampia kuin toiset, mikä voi auttaa sinua optimoimaan koodiasi.

## Katso myös
Kehittäessäsi Arduino-projektiasi voit hyödyntää myös muita vianmääritystyökaluja, kuten LED-valoja ja sarjaportin eri tavoin vilkkuvia viestejä. Lisäksi voit myös käyttää seurantahiirtä, joka auttaa hahmottamaan, missä kohdassa koodia olet parhaillaan ja milloin se suorittaa tiettyjä toimintoja.

- [Arduino sarjaporttikonsolin ohjeet](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino debuggaus video-opas (englanniksi)](https://www.youtube.com/watch?v=ipyD0X6IUOY)
- [Arduino forum](https://forum.arduino.cc/)