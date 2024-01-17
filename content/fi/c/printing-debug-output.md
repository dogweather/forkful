---
title:                "Virheenkorjaustulostuksen tulostaminen"
html_title:           "C: Virheenkorjaustulostuksen tulostaminen"
simple_title:         "Virheenkorjaustulostuksen tulostaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

Mitä ja miksi?

Debug-tulostaminen on tapa havainnollistaa ohjelman suoritusta tulosteiden avulla. Tämä auttaa ohjelmoijaa ymmärtämään, mitä ohjelma tekee kuhunkin kohtaan, ja mahdollisia virheitä, jotka voivat esiintyä. Se myös auttaa korjaamaan nämä virheet ja parantamaan koodin suorituskykyä.

Miten:

Debug-tulostamista varten, voimme käyttää ```C printf()```-funktiota, joka tulostaa halutun arvon tai tekstin konsoliin. Voimme myös käyttää ```C fprintf()```-funktiota, mikä antaa mahdollisuuden valita, minne tulostamme (esimerkiksi tiedostoon). 

Seuraavassa esimerkissä käytämme ```C printf()```-funktiota tulostamaan merkkijonon ja kokonaisluvun :

```C
#include <stdio.h>

int main()
{
    char *string = "Tämä on debug-tulostus";
    int number = 5;

    printf("%s\n", string);
    printf("Luku: %d\n", number);

    return 0;
}

```
Tämän esimerkin output:

```
Tämä on debug-tulostus
Luku: 5
```

Syvempi sukellus:

Debug-tulostamisella on pitkä historia ohjelmoinnissa. Ennen modernien kehitystyökalujen käyttöä, ohjelmoijien piti käyttää tulosteita tutkiessaan ohjelman suoritusta ja korjatessaan virheitä. Tänä päivänä on olemassa myös muita vaihtoehtoja debug-tulostamiselle, kuten käyttöliittymien ja debuggerien käyttö. 

Voit myös tutkia tarkemmin, miten ```C printf()```-funktio toimii, ja miten voit muotoilla tulostettavan tekstin, lisäämällä esimerkiksi muuttujien arvoja. 

## Linkit:

- [C printf() Dokumentaatio](https://en.cppreference.com/w/c/io/fprintf)
- [C debugging-opas](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)