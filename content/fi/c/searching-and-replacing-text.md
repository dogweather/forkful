---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen on prosessi, jossa voit paikallistaa määritetyn merkkijonon tai mallin ja korvata sen toisella merkkijonolla. Ohjelmoijat tekevät sen tehdäkseen koodistaan helpommin ylläpidettävän ja enhän tietokoneen suorituskykyä.

## Miten tehdä:

Alla on esimerkki merkkijonon etsimisestä ja korvaamisesta C-ohjelmoinnissa.

```C
#include <stdio.h>

void replaceChar(char* s, char oldChar, char newChar) {
   for (int i = 0; s[i] != '\0'; ++i) {
      if (s[i] == oldChar) {
         s[i] = newChar;
      }
   }
}

int main() {
   char str[] = "Hei maailma";
   printf("Ennen: %s\n", str);

   replaceChar(str, 'a', 'e');
   printf("Jälkeen: %s\n", str);

   return 0;
}
```

Tulostus:

```C
Ennen: Hei maailma
Jälkeen: Hei meeilme
```

## Sukellus syvälle

Tekstinhaku- ja korvaustoiminnot ovat olleet tietojenkäsittelyn peruselementtejä yli puoli vuosisataa ja niitä käytetään monissa yhteyksissä, kuten tietokoneen ohjeistoissa, tietojenkäsittelytehtävissä ja verkkohauissa. Vaihtoehtoisia menetelmiä tämän toiminnallisuuden toteuttamiseksi ovat muun muassa säännöllisten lausekkeiden käyttö tai tehokkaammat datastruktuurit, kuten trie tai aho-corasick automaatti. Tekstin etsinnän ja korvaamisen toteutus riippuu suuresti ohjelmoijan tarpeista, valituista työvälineistä ja käytettävissä olevista resursseista.

## Katso myös

- "Mastering Algorithms with C" by Kyle Loudon 
- ["Working with Strings in C"](https://developer.ibm.com/technologies/systems/tutorials/au-cstring/) by IBM Developer
- [The GNU C Library](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html) for more on string manipulation.