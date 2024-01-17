---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Arduino: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Säännöllisiä lausekkeita käytetään ohjelmoinnissa etsimään ja manipuloimaan tekstiä. Niiden avulla voit tehdä tarkkoja hakuja ja korvata osia tekstistä tietyin ehdoin. Ohjelmoijat käyttävät säännöllisiä lausekkeita vaivattomasti muuntamaan suuria määriä tekstiä tai tiedostoja kerralla.

## Miten:
### Haku
Arduino-kääntäjässä säännöllisiä lausekkeita käytetään ```Arduino Regex()``` -funktiolla. Funktio ottaa kaksi parametria: lausekkeen ja kohdetekstin. Tässä esimerkissä etsimme tekstistä "LED": ```Arduino
Arduino Regex ledPattern("LED");
```
Voit myös käyttää metakarakteria ```*``` merkitsemään kaikkia mahdollisia kirjaimia sen jälkeen. Tässä esimerkissä etsimme kaikki kirjaimet tekstin "LED" jälkeen: ```Arduino
Arduino Regex ledPattern("LED*");
```
### Korvaaminen
Regex-funktiolla voit myös korvata tekstistä haluamasi osat. Tässä esimerkissä haluamme korvata kaikki "LED"-sanat tekstillä "valo":```Arduino
String uusiTeksti = ledPattern.replace("valo"); 
```

Poiketen perinteisistä kielistä, kuten Java, Arduino Regex() -funktio ei palauta boolean-arvoa (totuusarvoa) vaan uuden String-tyyppisen tekstin. 

## Syväsukellus:
Säännölliset lausekkeet periytyvät 1950-luvun matematiikasta. Ajoittaisten lausekkeiden lisäksi on olemassa muita tapoja manipuloida tekstiä, kuten makkroja tai kiinteitä hakuja (string pattern matching).

## Katso myös:
- [RegularExpression.com](https://www.regular-expressions.info/tutorial.html)
- [Arduino Reference - Regex()](https://www.arduino.cc/reference/en/language/functions/regular-expressions/regex/)