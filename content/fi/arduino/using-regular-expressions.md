---
title:                "Arduino: Säännöllisten lausekkeiden käyttö"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Miksi käyttää säännöllisiä lausekkeita Arduino-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja Arduino-ohjelmoinnissa, sillä ne mahdollistavat tietyntyypisen tiedon löytämisen ja käsittelyn merkkijonoista. Ne ovat erityisen hyödyllisiä silloin, kun tiettyyn kuvioon tai muotoon perustuvan tiedon etsiminen olisi muuten haastavaa tai aikaa vievää.

# Miten käyttää säännöllisiä lausekkeita Arduino-ohjelmoinnissa?

Arduino-ohjelmointiympäristössä on sisäänrakennettu RegExp-kirjasto, joka mahdollistaa säännöllisten lausekkeiden käytön. Voit käyttää sitä etsimään ja muokkaamaan merkkijonoja Arduino-mikrokontrollerillasi.

### Esimerkki:

```Arduino
#include <RegExp.h>

String data = "Ostin #3 perunaa tänään kaupasta.";
String pattern = "\\#(\d+)";
RegExp regex(pattern);

if(regex.find(data)){
  Serial.println("Löysin " + regex.match() + " perunaa.");
}
```

Tässä esimerkissä etsimme merkkijonosta "data" säännöllisen lausekkeen avulla numeron "#"-merkin takaa ja tulostamme löydetyn numeron sarjan "Serial"-monitorille. Tulos olisi "Löysin 3 perunaa."

### Tulos "Serial"-monitorilla:

```
Löysin 3 perunaa.
```

# Syventävä sukellus säännöllisten lausekkeiden käyttöön Arduino-ohjelmoinnissa

Säännöllisillä lausekkeilla on laaja käyttöalue Arduino-ohjelmoinnissa, ja niitä voidaan käyttää esimerkiksi tiedon validointiin, analysointiin ja muokkaamiseen. Ne mahdollistavat monimutkaisempien merkkijonojen käsittelyn ja auttavat ohjelmoijia tekemään tarkempia ja luotettavampia ohjelmia.

On myös tärkeää huomata, että säännölliset lausekkeet voivat olla hyödyllisiä myös muilla ohjelmointikielillä, kuten C++, joten oppimalla niiden käytön Arduino-ohjelmoinnissa, voit parantaa ohjelmointitaitojasi yleisesti.

# Katso myös

- Regex-kirjasto [Arduino-verkkosivuilla](https://www.arduino.cc/reference/en/libraries/regexp/)
- Säännöllisten lausekkeiden opas [W3Schools-sivustolla](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- Interaktiivinen [säännöllisten lausekkeiden harjoittelusivusto](https://regexone.com/)