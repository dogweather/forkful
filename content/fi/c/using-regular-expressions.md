---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "C: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Säännöllisten lausekkeiden käyttäminen on tapa löytää ja käsitellä tietoa tarkasti ja tehokkaasti. Koodareiden on usein tarpeen suodattaa tai muokata tietoa monimutkaisten sääntöjen mukaan, ja tällöin säännöllisistä lausekkeista (tunnetaan myös nimellä regex tai regexp) on suuri apu. Säännölliset lausekkeet ovat jousiterävä työkalu jokaisen ohjelmoijan työkalupakissa.

## Näin teet sen: 

Näyttävien esimerkkien kera opimme helposti, miten säännölliset lausekkeet toimivat. Katso alla olevaa koodia ja sen tuottamaa tietoa. Voit kokeilla myös itse kirjoittamalla koodin ajettavaksi.

```C 
#include <stdio.h>
#include <regex.h>

int main() {
  // Määritellään sääntö, joka tunnistaa "Hei" sanat
  regex_t regex;
  regcomp(&regex, "Hei", 0);

  // Etsitään säännön määrittelemaä tekstiä
  char txt[] = "Hei, mitä kuuluu?";
  int result = regexec(&regex, txt, 0, NULL, 0);

  // Tulostetaan tulos
  if (!result){
    printf("Löytyi!\n");
  } else {
    printf("Eipä löytynyt.\n");
  }

  return 0;
}
```
Tulostus:
```
Löytyi!
```

## Sukella syvemmälle:
Säännölliset lausekkeet ovat olleet käytössä jo yli 60 vuotta ja ovat edelleen tärkeä osa ohjelmointia. Niihin voi tutustua esimerkiksi lukemalla Brian Kernighanin ja Rob Piken artikkelin "Run-Time Pattern Matching", jossa esiteltiin ensimmäinen säännöllisiä lausekkeita käyttävä ohjelmointikieli. Nykyään on olemassa myös muita vaihtoehtoja, kuten awk ja sed, jotka on suunniteltu erityisesti säännöllisten lausekkeiden käyttämiseen. C:n regex-kirjasto on myös usein verrattuna muihin ohjelmointikieliin, kuten Pythoniin, jossa säännöllisten lausekkeiden käyttäminen on helppoa.

## Katso myös:
- [Brian Kernighanin ja Rob Piken artikkeli "Run-Time Pattern Matching"](https://www.cs.princeton.edu/courses/archive/spring09/cos333/beautiful.html)
- [Document Foundation:n dokumantaatio säännöllisistä lausekkeista C:ssä](https://docs.libreoffice.org/sfx2/html/regex_8h.html)