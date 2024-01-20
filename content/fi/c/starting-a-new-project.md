---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelmiston tai sovelluksen kehittämistä tyhjästä. Ohjelmoijat tekevät näitä projekteja ratkaistakseen erityisiä ongelmia tai tarpeita.

## Kuinka Näin:

Tässä on esimerkki uuden C-projektin luomiseen. Saatat haluta luoda "main.c" tiedoston tällä koodilla:

```C
#include <stdio.h>

int main() {
  printf("Hei Maailma!\n");
  return 0;
}
```

Suorita `gcc main.c -o ohjelma` komennolla, niin tämä kääntää koodin. Kokeile sitä suorittamalla `./ohjelma`. Tulostuksen pitäisi olla:

```C
Hei Maailma!
```

## Syvemmältä:

1. _Historiallinen yhteys_: C-ohjelmointikieli on peräisin 1970-luvulta ja sitä on käytetty laajasti järjestelmä- ja sovelluskehityksessä. 
   
2. _Vaihtoehdot_: Voit aloittaa uuden projektin myös muilla ohjelmointikielillä, kuten Java, Python tai JavaScript, riippuen projektin tarpeista.
   
3. _Toteutuksen yksityiskohdat_: C-ohjelmointikielessä käytämme `#include <stdio.h>` standardikirjastoa tulostus- ja syötefunktioille. "main" on ohjelman suorituksen aloitusfunktio ja `return 0;` ilmaisee ohjelman onnistuneen päättymisen.

## Katso myös:

- [GeeksForGeeks: "C Programming Language"](https://www.geeksforgeeks.org/c-programming-language/)