---
title:    "Arduino: Tekstin etsiminen ja korvaaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaa olla tietoinen tekstien etsimisestä ja korvaamisesta (search and replace) Arduino-ohjelmoinnissa? Se on tärkeä taito, joka auttaa sinua muokkaamaan ja korjaamaan koodiasi nopeasti ja tehokkaasti. Se myös helpottaa monimutkaisten muutosten tekemistä useissa tiedostoissa yhtäaikaisesti.

## Kuinka tehdä

Etsimisen ja korvaamisen käyttäminen Arduino-ohjelmoinnissa on helppoa. Seuraa alla olevia ohjeita ja esimerkkejä, niin olet pian täysin kärryillä.

### Etsiminen ja korvaaminen viestissä

Jos haluat etsiä tiettyä tekstiä viestissä ja korvata sen toisella, voit käyttää `replace`-funktiota. Seuraavassa esimerkissä etsimme tekstiä "hello" ja korvaamme sen tekstillä "hi".

```Arduino
String viesti = "hello world";
viesti.replace("hello", "hi");
Serial.println(viesti); // tulostaa "hi world"
```

### Etsiminen ja korvaaminen tiedostossa

Jos haluat etsiä ja korvata tekstiä tiedostossa, voit käyttää `searchAndReplace`-funktiota. Seuraavassa esimerkissä etsimme ja korvaamme kaikki "A" kirjaimet tiedostosta "kirjaimet.txt".

```Arduino
File tiedosto = SD.open("kirjaimet.txt");
String sisalto = tiedosto.readString();
tiedosto.close();

sisalto = searchAndReplace(sisalto, "A", "B");

tiedosto = SD.open("kirjaimet.txt", FILE_WRITE);
tiedosto.print(sisalto);
tiedosto.close();
```

## Syvempään

Etsiminen ja korvaaminen toimivat hyvin yksinkertaisissa tapauksissa, mutta mitä tapahtuu, kun haluat tehdä monimutkaisempia muutoksia? Tässä muutamia vinkkejä, jotka auttavat sinua syvällisemmin ymmärtämään etsimistä ja korvaamista Arduino-ohjelmoinnissa.

- Voit käyttää säännöllisiä lausekkeita etsimisen ja korvaamisen yhteydessä. Tämä auttaa sinua löytämään ja korvaamaan monimutkaisempia tekstejä.
- Voit myös käyttää muuttujia tai laskutoimituksia korvaavassa tekstissä, jotta voit muokata sitä dynaamisesti.
- Etsimistä ja korvaamista voidaan käyttää myös muiden Arduinon kirjastojen kanssa, kuten SD-kortin ja Ethernet-yhteyden käsittelyyn.

## Katso myös

- [Regular expressions in C++](https://www.regular-expressions.info/cpp.html)
- [String functions in Arduino references](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino SD library reference](https://www.arduino.cc/en/reference/SD)
- [Arduino Ethernet library reference](https://www.arduino.cc/en/Reference/Ethernet)