---
title:                "Merkkijonon muuttaminen suuriksi kirjaimiksi"
html_title:           "C: Merkkijonon muuttaminen suuriksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen suuriksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Stringien kapselointi on hyödyllinen tekniikka, kun käsitellään tekstipohjaista dataa, kuten käyttäjän syötteitä tai tiedostojen nimiä. Se mahdollistaa johdonmukaisen muotoilun ja helpottaa tietojen käsittelyä.

## Näin

```C
#include <stdio.h>
#include <string.h>

// Function to capitalize a string
void capitalize(char *str) {
    int i = 0;

    // Loop through each character
    while (str[i] != '\0') {
        // Check if character is lowercase
        if (str[i] >= 'a' && str[i] <= 'z') {
            // Convert to uppercase by subtracting 32 from ASCII value
            str[i] = str[i] - 32;
        }
        i++;
    }

    printf("Capitalized string: %s", str);
}

int main() {
    char str[50];
    printf("Enter a string: ");
    scanf("%s", str);

    // Call capitalize function
    capitalize(str);

    return 0;
}
```

**Syöte:** hello
**Output:** Capitalized string: HELLO

## Tarkempi selvitys

Stringien kapselointi on yksinkertainen prosessi, joka muuttaa kaikki merkit pienaakkosista suuraakkosiin. Tämä voidaan tehdä käymällä läpi jokainen merkki ja muuttamalla sen ASCII-arvoa. ASCII-taulukossa pienaakkoset alkavat numerolla 97 ja suuraakkoset numerolla 65. Joten pienaakkoset voidaan muuttaa suuraakkosiksi vähentämällä 32 niiden ASCII-arvosta.

Kuitenkin tämä yksinkertainen prosessi toimii vain englannin kielellä, jossa on vain 26 kirjainta. Jos haluat kapseloida muita merkkejä, kuten ääkkösiä tai erikoismerkkejä, sinun on käytettävä monimutkaisempia toimenpiteitä, kuten merkistöjen muunnosta.

## Katso myös

- [ASCII-taulukko](https://fi.wikipedia.org/wiki/ASCII)
- [Merkistöjen muunnos C:ssä](https://www.tutorialspoint.com/c_standard_library/c_function_setlocale.htm)
- [Stringien muokkaus C:ssä](https://www.geeksforgeeks.org/c-program-basics-string-operations/)