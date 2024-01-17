---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Etsiminen ja korvaaminen tekstillä on tärkeä ohjelmointikäytäntö, joka auttaa ohjelmoijia muokkaamaan suuria määriä koodia nopeasti ja tehokkaasti. Kun etsit tekstiä ja korvaat sen uudella, voit muuttaa helposti monia koodeja kerralla ja välttää virheet, jotka voivat syntyä manuaalisella muokkaamisella. 

## Miten:

Ehkä yksinkertaisin tapa etsiä ja korvata tekstiä on käyttää `replace()` -funktiota Arduino-koodissa. Voit käyttää tätä funktiota esimerkiksi vaihtaaksesi vanhoja sanoja uusiin tai muuttaaksesi tiettyjä lauseita ohjelmassasi. 

```
Arduino.replace("vanha sana", "uusi sana");
```

Tämä koodi korvaa kaikki esiintymät "vanha sana" uudella sanalla "uusi sana". Voit myös muuttaa vain osan sanasta käyttämällä sijoitusrunkoa, joka näyttää tältä: 

```
Arduino.replace("vanha sana", "uusi sana", index);
```

Tässä index on kohta, josta haluat aloittaa tekstinhaku ja korvaaminen. Tämä antaa sinulle enemmän joustavuutta ja tarkkuutta muokkaamisessa. 

## Syvä sukellus:

Hakemisen ja korvaamisen käytäntö on ollut olemassa jo vuosikymmeniä ja se on edelleen tärkeä osa nykypäivän ohjelmointia. On myös olemassa muita vaihtoehtoja, kuten regular expression -käsittelyt tai ohjelmointitarkkuusohjelmat, jotka voivat tehdä monimutkaisempia tekstinhakuja ja korvauksia. 

Arduino tarjoaa myös muita hyödyllisiä funktioita, kuten `replaceAll()` ja `replaceFirst()`. Näiden avulla voit tehdä korvauksia koko koodisi tai vain ensimmäisen esiintymän aikana. Voit myös säätää hakukriteerejä, kuten kirjainkoolla. 

## Katso myös:

Voit lukea lisää tekstinhauista ja korvaamisista Arduino-ohjelmoinnissa Arduino-oppaastamme (linkki: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/). Voit myös löytää hyödyllisiä vinkkejä ja neuvoja Arduino-yhteisöstä ja foorumeilta (linkki: https://forum.arduino.cc/).