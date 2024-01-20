---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsennys merkkijonosta on merkkijonon muuttamista päivämääräobjektiksi. Ohjelmoijat tekevät tämän, jotta voidaan käsitellä päivämäärätietoja helpommin ja tehokkaammin.

## Näin teet:

Tässä on esimerkkikoodi päivämäärän jäsennyksestä Arduino-ympäristössä:

```Arduino 
#include <TimeLib.h>
#define TIME_MSG_LEN 11 
#define TIME_HEADER  'T'   

void setup() {   
  Serial.begin(9600);
}

void loop() {   
  processSyncMessage();
} 

void processSyncMessage() {
  String receivedString = "T1624389600"; 
  if(receivedString.length() == TIME_MSG_LEN){
    if(receivedString[0] == TIME_HEADER) {
      time_t pctime;
      pctime = (time_t)receivedString.substring(1).toInt();
      setTime(pctime);   
      Serial.println(year());   
      Serial.println(month());  
      Serial.println(day());
      Serial.println(hour());  
      Serial.println(minute()); 
      Serial.println(second());
    }
  }
}    
```

Tämän ohjelman tulostus saattaa näyttää seuraavalta:

```Arduino
2021
6
22
17
40
0
```
## Sukellus syvyyksiin

Päivämäärän tulkitseminen merkkijonosta on perinteinen tehtävä, joka liittyy lukemattomiin sovelluksiin, kuten tapahtumien ajastukseen. Sinulla on erilaisia tapoja toteuttaa tämä, kuten manuaalinen tulkitseminen tai luotettavan kirjaston, kuten TimeLibin, käyttäminen, joka on esitetty yllä. TimeLib-kirjasto tarjoaa monipuolisen kokoelman päivämäärä- ja aikatoimintoja, jotka auttavat sinua selviämään vaativista tehtävistä. 

TimeLib-kirjastossa päivämäärän jäsennys saavutetaan muuntamalla tiettyyn aikaan liittyvä merkkijono kokonaislukuarvoon (UNIX-aikaleimaan) ja asettamalla tämä arvo järjestelmän kellonaikaan. Tämä menetelmä on yksinkertainen, tehokas, ja se toimii hyvin pienillä mikro-ohjaimilla, kuten Arduinolla.

## Lisätietoja

Jos haluat tutustua tarkemmin Arduino-ohjelmointiin ja päivämäärän jäsennyksen, tutustu seuraaviin linkkeihin:

- [Arduino virallinen verkkosivusto](https://www.arduino.cc/)
- [TimeLib-kirjaston GitHub-sivu](https://github.com/PaulStoffregen/Time)
- [Tutorial on Date and Time functions in Arduino](https://startingelectronics.org/software/arduino/date-time-arduino/)