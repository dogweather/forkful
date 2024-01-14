---
title:    "Arduino: Mallin mukaiseen kaavaan sopivien merkkien poistaminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa, Arduinossa? On monia syitä, miksi tämä voi olla tarpeellista. Esimerkiksi, jos haluat puhdistaa tekstisyötteen anomaliasta tai käsitellä herkästi henkilökohtaisia tietoja, poistaminen voi olla hyödyllistä.

## Kuinka tehdä se

Poistaaksesi merkkejä kirjojen tai kaavojen vastaavalla tavalla Arduinossa, tarvitset `String`kirjastoa. Voit sitten käyttää `indexOf()` ja `replace()`-funktiota löytääksesi ja korvataksesi vastaavat merkit. Katso alla oleva koodiesimerkki:

```Arduino
String teksti = "Tämä on esimerkki$ tekstisyöttesteestä!";
// Etsii merkin '$' indeksissä
int indeksi = teksti.indexOf("$");
// Korvaa merkin '.':lla
teksti.replace(indeksi, 1, ".");
// Tulostaa muokatun tekstin
Serial.println(teksti);

// Tulostaa: "Tämä on esimerkki. tekstisyöttestä!"
```

Sinun tulee myös huomioida, että jos haluat poistaa useampia merkkejä, voit käyttää `replace()`-funktion sijaan `remove()`-funktiota.

## Syväsyventyminen

Jokaisella merkillä on ASCII-numero, josta voit tarkistaa, vastaako se haluamaasi kaavaa. Voit myös käyttää laajennettuja ASCII-merkkejä löytääksesi ja poistaaksesi tarkemmin merkkejä. Toinen vaihtoehto on käyttää säännöllisiä lausekkeita poistamiseen. Näihin syventymisen tapoihin pääset lisätietoa hakemalla.

## Katso myös

- [String-kirjastodokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII-taulukko](https://www.asciitable.com/)
- [Säännöllisten lausekkeiden opas](https://www.regular-expressions.info/)