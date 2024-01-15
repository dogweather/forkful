---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Arduino: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

On olemassa monia syitä, miksi voit luoda väliaikaisen tiedoston Arduino-projektillesi. Yksi syy voi olla tallentaa väliaikaisia ​​tietoja, kuten käyttäjän syötteitä, jotka eivät ole välttämättömiä ohjelman lopullisessa versiossa. Tiedoston luominen voi myös auttaa ohjelman sisäisessä järjestelyssä ja suorituskyvyssä.

## Näin teet sen

```Arduino

// Luodaan väliaikainen tiedosto nimeltä "data.txt"
File data = SD.open("data.txt", FILE_WRITE);

// Kirjoitetaan tiedostoon merkkijono "Hei maailma!"
data.println("Hei maailma!");

// Suljetaan tiedosto
data.close();

```

Koodiesimerkissä luodaan väliaikainen tiedosto nimeltä "data.txt" ja kirjoitetaan siihen merkkijono "Hei maailma!". Tiedosto suljetaan lopuksi.

## Syvemmälle asiassa

On tärkeää huomata, että väliaikaisen tiedoston luominen vie tilaa Arduino-piirin muistista. Sen sijaan, että tallentaisit tiedoston lopullisen version ohjelmassa, voit myös käyttää esimerkiksi muuttujia ja taulukoita tallentamaan väliaikaisia ​​tietoja. Näin säästät tilaa muistissa ja saat saman lopputuloksen.

## Katso myös

- Opas Arduino-piirin muistin optimointiin: [https://arduino.cc/memoryoptimization](https://arduino.cc/memoryoptimization)
- SD-kirjaston dokumentaatio: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)