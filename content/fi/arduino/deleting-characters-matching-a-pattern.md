---
title:                "Arduino: Merkkijonon hahmojen poistaminen vastaavien kaavojen perusteella"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miksi
On monia syitä miksi saattaisit haluta poistaa tietyn kuvion mukaisia merkkejä, kuten turhia numeroita tai kirjaimia datasta. Tämä on hyödyllistä esimerkiksi sensoridataa käsitellessä, jossa haluat ehkä poistaa ylimääräisiä merkkejä ennen kuin käytät niitä laskennassa tai tallennat ne muistiin.

# Miten
Poistaminen merkkejä määrätyn kuvion mukaan on helppoa käyttäen Arduinoa ja String -kirjastoa. Seuraava koodiesimerkki näyttää yksinkertaisen skriptin, joka poistaa kaikki numerot String -muuttujasta:

```Arduino
String data = "Tämä on esimerkkidataa 123";
data = data.replaceAll("[0-9]", "");
Serial.println(data);
```

Tässä esimerkissä käytämme `replaceAll` -funktiota ja säännöllistä lauseketta `[0-9]`, joka tarkoittaa kaikkia numeroita välillä 0-9. Tämä korvaa kaikki numerot tyhjällä merkkijonolla, jolloin ne poistetaan. Voit muokata säännöllistä lausetta tarpeidesi mukaan.

# Syvemmälle
Tehokas tapa poistaa merkkejä määrätyn kuvion mukaan on käyttää säännöllisiä lausekkeita. Tässä on muutamia esimerkkejä:

- `[0-9]` - korvaa kaikki numerot
- `[a-z]` - korvaa kaikki pienet kirjaimet
- `[A-Z]` - korvaa kaikki isot kirjaimet
- `[^a-zA-Z]` - korvaa kaikki merkit, jotka eivät ole kirjaimia
- `.+` - korvaa kaikki merkkijonon merkit

Säännöllisiä lausekkeita käyttämällä voit poistaa haluamasi merkkitaulukon mukaan.

# Katso myös
- [String -kirjaston dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Säännöllisten lausekkeiden opas](https://www.regular-expressions.info/)
- [Arduino -ohjelmoinnin opas (suomeksi)](https://rambo.fi/arduino-ohjelmointi-opas-osa-1-ledien-ohjaus/)