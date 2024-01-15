---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "C: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Random-numeroiden luominen on erittäin hyödyllinen taito C-ohjelmoijille. Se voi parantaa ohjelman eri osien monimutkaisuutta ja tarjota entistä monipuolisempia toimintoja, kuten satunnaisesti valittuja käyttäjänimiä tai salasanoja.

## Miten Tehdä

Random-numeroiden luominen C-kielellä on melko helppoa. Käytämme tähän tarkoitukseen <stdlib.h> kirjastoa, joka sisältää joukon funktioita satunnaislukujen luomiseen.

**Esimerkki 1:** Luo random-numero välillä 0-10 ja tulosta se.

```C 
#include <stdlib.h> 
#include <stdio.h> 

int main() 
{ 
    // Käytämme funktiota rand() generoimaan random-luvun
    // ja % käyttäämään modulo-operaatiota jakaaksemme luvun halutulla välillä
    int random = rand() % 11; 
    
    // Tulostetaan random-numero
    printf("Satunnainen numero välillä 0-10: %d", random); 
    
    return 0; 
} 
```
**Tulos:**

```
Satunnainen numero välillä 0-10: 6
```

Tai voimme käyttää myös funktiota srand() asettamaan alkuarvo rand() -toiminnolle, jotta saamme erilaisia tuloksia joka kerta kun ohjelmaa ajetaan.

**Esimerkki 2:** Luo ja tulosta 10 random-numeroa välillä 1-1000.

```C 
#include <stdlib.h> 
#include <stdio.h> 

int main() 
{ 
    // Asetetaan arvo rand() -funktiolle
    srand(1234); 
    
    // Luodaan 10 random-numeroa ja tulostetaan ne
    for (int i = 0; i < 10; i++) { 
        int random = rand() % 1001; 
        printf("Random numero: %d\n", random); 
    } 
    
    return 0; 
} 
```
**Tulos:**

```
Random numero: 179  
Random numero: 110 
Random numero: 794 
Random numero: 644 
Random numero: 626 
Random numero: 146 
Random numero: 887 
Random numero: 514 
Random numero: 540 
Random numero: 848
```

## Syvemmälle

Random-numeroiden luominen perustuu siemenarvoon, jota käytetään rand() -funktion ensimmäisen arvon määrittämisessä. Tavallisesti tämä siemenarvo määräytyy käytetyn ajan perusteella, joten kaksi ohjelmaa, jotka ajetaan samanaikaisesti, saavat saman tuloksen. Tämän välttämiseksi voimme käyttää funktiota srand() asettamaan siemenarvon haluamallamme tavalla.

On myös huomionarvoista, että rand() -funktio generoi pseudosatunnaisia numeroita eli lukujonoja, jotka näyttävät randomilta, mutta ovat todellisuudessa ennaltamääritettyjä. Tästä syystä on suositeltavaa käyttää myös muita lähteitä, kuten aikaleimoja tai käyttäjän syöttöä, vaikuttamaan rand() -funktion toimintaan ja saamaan mahdollisimman randomin luvun.

## Katso Myös

- [C: Random Numbers](https://www.programiz.com/c-programming/c-random-number-generation)
- [Generating Random Numbers in C](https://www.cprogramming.com/tutorial/random.html)
- [stdlib.h](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)