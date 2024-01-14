---
title:    "Arduino: Kirjoittaminen standardivirheeseen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi kirjoittaa standard erroriin Arduino-ohjelmoinnissa? Se on hyvä kysymys. Standard error on virtuaalinen kanava, jota käytetään ohjelmointivirheiden ja muunlaisen diagnostiikkainformaation tallentamiseen. Tämä tarkoittaa, että tulostaminen standard erroriin auttaa sinua ymmärtämään ja korjaamaan projektisi virheitä.

## Miten

Tässä esimerkiksi koodinpätkä, joka näyttää, miten kirjoittaa standard erroriin Arduino-ohjelmoinnissa:

```Arduino
int x = 10;
if (x > 5) {
  Serial.println("X on suurempi kuin 5");
} else {
  Serial.println("X ei ole suurempi kuin 5");
}
```
Kun tämä koodi ajetaan, standard erroriin tulostuu joko "X on suurempi kuin 5" tai "X ei ole suurempi kuin 5" riippuen siitä, mikä ehto on totta. Voit myös käyttää standard erroria tulostamaan muuttujien arvoja tai muita tärkeitä tietoja suoraan ohjelmasta.

## Syvällisempi sukellus

Standard errorin käyttäminen voi olla todella hyödyllistä etenkin monimutkaisemmissa projekteissa. Se auttaa sinua selvittämään, missä kohdassa ohjelmaasi on virhe ja saat tärkeää tietoa ohjelman suorituskyvystä. On tärkeää muistaa, että standard errorin käyttö ei korvaa hyvää ohjelmointia ja ohjelman virheitä tulisi aina pyrkiä korjaamaan, mutta se voi olla hyödyllinen työkalu virheiden jäljittämiseen ja korjaamiseen.

## Katso myös
- [Arduino-ohjelmointiopas](https://www.arduino.cc/en/Guide/)
- [Standard errorin käyttö muissa ohjelmointikielissä](https://www.tutorialspoint.com/cplusplus/cpp_error_handling.htm)
- [Virheenhallinta Arduino-ohjelmoinnissa](https://www.instructables.com/Arduino-Error-Handling/)