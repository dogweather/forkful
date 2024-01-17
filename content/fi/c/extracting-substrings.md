---
title:                "Alirivien poimiminen"
html_title:           "C: Alirivien poimiminen"
simple_title:         "Alirivien poimiminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Olet varmasti törmännyt koodissasi tarpeeseen hakea tietty osa merkkijonosta tai sanasta. Tätä kutsutaan alimerkkijonojen erotteluksi ja se on tärkeä osa ohjelmointia. Alimerkkien erottelu on hyödyllistä esimerkiksi silloin, kun haluat tutkia tarkemmin merkkijonon sisältöä tai vertailla sanoja keskenään.

## Kuinka tehdä?

Alimerkkien erottelu onnistuu helposti C-kielellä käyttäen sisäänrakennettua ```strstr()``` -funktiota. Tämä funktio tarkistaa annetusta merkkijonosta halutun alimerkkijonon ja palauttaa sen sijainnin tai ```NULL```, jos alimerkkijonoa ei löydy. Esimerkiksi:

```
char str[] = "Tervetuloa maailma!";
char *substring = "maailma";

char *ptr = strstr(str, substring);
printf("%s", ptr);
```

Tulosteena saamme: ```maailma!```

## Syvemmälle

Alimerkkien erottelu ei ole uusi asia, vaan se on ollut käytössä jo vuosikymmenien ajan. Aikaisemmin siihen käytettiin monia eri tapoja, kuten ```strtok()``` -funktiota, joka erotteli merkkijonon eri osiin ja palautti ne taulukkona. Nykyään suositeltava tapa on kuitenkin käyttää ```strstr()``` -funktiota sen yksinkertaisuuden ja luotettavuuden takia.

## Katso myös

Lisätietoa alimerkkien erottelusta ja muista hyödyllisistä C-kielessä käytettävistä funktioista löydät [täältä](https://www.programiz.com/c-programming/c-library-function).