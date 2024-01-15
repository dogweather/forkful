---
title:                "Tulostaminen debuggaustuloste"
html_title:           "C: Tulostaminen debuggaustuloste"
simple_title:         "Tulostaminen debuggaustuloste"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-syötteiden tulostaminen on C-ohjelmoinnissa tärkeä työkalu, joka auttaa kehittäjiä paikantamaan ja korjaamaan virheitä. Näiden tulosteiden avulla voit tarkastella ohjelman suoritusaikaa ja selvittää, missä kohdassa koodia tapahtuu virhe. Se on myös hyvä tapa tarkistaa muuttujien arvoja ja varmistaa, että ohjelma toimii odotetulla tavalla.

## Miten

C-kielen debug-syötteiden tulostaminen on helppo tehdä käyttämällä `printf` -funktiota. Voit tulostaa muuttujien arvoja laittamalla ne lainausmerkkien sisään ja käyttämällä `%d` tai `%f` -merkkiä osoittamaan, että kyseessä on kokonaisluku tai liukuluku. Voit myös käyttää `\n` -merkkiä tulostuksen välissä luodaksesi uuden rivin tekstiin.

```
int numero = 10;
float desimaaliluku = 3.14;
printf("Muuttujien arvot ovat %d ja %.2f.\n", numero, desimaaliluku);
```

Tämä tulostaa seuraavan tekstin:

```
Muuttujien arvot ovat 10 ja 3.14.
```

Voit myös tulostaa muuttujien arvoja otsikoiden kanssa käyttämällä `printf` koodia, kuten seuraavassa esimerkissä:

```
printf("Numero: %d\n" "Desimaaliluku: %.2f\n", numero, desimaaliluku);
```

Tämä tulostuu tekstinä:

```
Numero: 10
Desimaaliluku: 3.14
```

## Syvemmälle

Debug-syötteiden tulostaminen on erittäin hyödyllistä, kun haluat selvittää, miten koodisi suorittuu, mutta muista poistaa kaikki tulosteet lopullisesta koodistasi. Voit tehdä tämän kommentoimalla koodirivejä tai käyttämällä `#ifdef` ja `#endif` -lausekkeita.

Voit myös käyttää `fprintf` -funktiota tulostamaan tietoa tiedostoon. Tämä voi olla hyödyllistä, kun haluat tallentaa debug-syötteitä pidemmäksi ajaksi.

```
FILE *tiedosto = fopen("debug.txt", "w");
fprintf(tiedosto, "Numero: %d\n" "Desimaaliluku: %.2f\n", numero, desimaaliluku);
fclose(tiedosto);
```

## Katso myös

- [C-kielen debuggaus](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [C-ohjelman suorituksen aikainen debuggaus](https://www.geeksforgeeks.org/inspecting-c-program-debug-in-c/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään debug-syötteiden tulostamisen tärkeyttä C-ohjelmoinnissa. Muista käyttää tätä tärkeää työkalua hienosäätämään koodiasi ja korjaamaan virheitä. Onnea ohjelmointiin!