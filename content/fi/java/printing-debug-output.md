---
title:                "Tulostaminen virheenjäljitystulosteet"
html_title:           "Java: Tulostaminen virheenjäljitystulosteet"
simple_title:         "Tulostaminen virheenjäljitystulosteet"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Printtaaminen tarkoittaa koodin suorituksen aikana tulostamista näytölle. Kehittäjät tekevät tätä yleensä virheiden korjaamiseksi ja ohjelman toiminnan tarkkailuksi.

## Miten:

```Java
System.out.println("Hello World!"); 
```

Tämä koodirivi tulostaa "Hello World!" näytölle konsolissa. Voit myös tulostaa muuttujien arvoja näytölle näin:

```Java
int numero = 5;
System.out.println(numero); 
```

Tämä tulostaisi "5" näytölle. Lisäksi voit myös käyttää "print" sijasta "println" jos haluat tulostaa ilman rivinvaihtoa konsolissa.

## Syväkurkistelu:

Printtaaminen on ollut tärkeä osa kehittäjien työkalupakkia jo vuosia. Ennen kuin nykyaikaiset debuggaustyökalut tulivat saataville, printtaaminen oli yleisin tapa tunnistaa ohjelmassa olevia virheitä. Nykyään on olemassa myös muita tapoja korjata ohjelman virheitä, kuten yksikkötestaaminen ja debuggausohjelmat.

## Katso myös:

[Java-tutoriaali: Debuggaus](https://www.tutorialspoint.com/java/java_debugging.htm)