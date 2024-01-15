---
title:                "Mallia vastaavien merkkien poisto"
html_title:           "C: Mallia vastaavien merkkien poisto"
simple_title:         "Mallia vastaavien merkkien poisto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi poistaa merkkejä jotka vastaavat tiettyä kaavaa? Yksi syy voi olla tarve muuttaa datan rakennetta, esimerkiksi poistamalla koodin tyhjät välit, jotka eivät ole välttämättömiä datan käsittelyn kannalta. Tämä voi myös auttaa järjestämään ja parantamaan datan luettavuutta.

## Kuinka tehdä

Alla on kaksi esimerkkiä, joissa poistetaan merkkejä jotka vastaavat tiettyä kaavaa C-ohjelmoinnissa. Voit käyttää näitä esimerkkejä pohjana oman koodin kirjoittamiselle.

```C
#include <string.h>
#include <stdio.h>

// Esimerkki 1: Poistetaan merkkejä jotka vastaavat tiettyä kaavaa

int main() {
    // Alustetaan merkkijono, josta halutaan poistaa merkit
    char merkkijono[] = "Hei! ## Tämä ## on ## esimerkki ## poistamisesta.";
    // Alustetaan kaava, johon halutaan poistaa vastaavat merkit
    char kaava[] = "##";
    // Alustetaan apumuuttuja, joka pitää kirjaa uudesta merkkijonosta
    char uusi_merkkijono[50];
    // Alustetaan laskuri
    int i = 0;
    int j = 0;
    
    // Käydään läpi merkkijono merkki kerrallaan
    while(merkkijono[i] != '\0') {
        // Jos merkki ei vastaa kaavaa, lisätään se uuteen merkkijonoon ja siirrytään seuraavaan merkkiin
        if(merkkijono[i] != kaava[0]) {
            uusi_merkkijono[j] = merkkijono[i];
            j++;
        // Jos merkki vastaa kaavaa, siirrytään seuraavaan merkkiin ilman, että mitään lisätään uuteen merkkijonoon
        } else {
            i++;
            continue;
        }
        i++;
    }
    // Lisätään lopputilan merkki
    uusi_merkkijono[j] = '\0';
    
    // Tulostetaan uusi merkkijono
    printf("Uusi merkkijono: %s\n", uusi_merkkijono);

    return 0;
}

/*Tulostus:
Uusi merkkijono: Hei! Tämä on esimerkki poistamisesta.
*/
```

```C
#include <string.h>
#include <stdio.h>

// Esimerkki 2: Poistetaan merkkejä tietyn alueen välistä

int main() {
    // Alustetaan merkkijono, jossa on sanoja, joista halutaan osa
    char merkkijono[] = "Tämäkinteksti on ## ylimääräistä ## tekstiä, jota ei tarvita.";
    // Alustetaan apumuuttuja, joka pitää kirjaa uudesta merkkijonosta
    char uusi_merkkijono[50];
    // Alustetaan laskuri
    int i = 0;
    int j = 0;
    
    // Käydään läpi merkkijono merkki kerrallaan
    while(merkkijono[i] != '\0') {
        // Jos merkki ei vastaa alueen kaavaa, lisätään se uuteen merkkijonoon ja siirrytään seuraavaan merkkiin
        if(merkkijono[i] != '#') {
            uusi_merkkijono[j] = merkkijono[i];
            j++;
        // Jos merkki vastaa alueen kaavaa, siirrytään seuraavaan merkkiin, mutta lisätään ensin välilyönti
        } else {
            uusi_merkkij