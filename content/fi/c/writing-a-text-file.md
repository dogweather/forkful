---
title:                "C: Tekstitiedoston kirjoittaminen"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen ohjelmoidessa on olennainen taito, joka auttaa sinua tallentamaan ja jakamaan tietoa ohjelmistojen kanssa. Se on myös yksi perusteellisista taidoista oppia C-ohjelmointia.

## Kuinka

Käytä "fopen()" -toimintoa luomaan tiedostonimikkeen ja avataksesi tiedoston. Käytä sitten "fprintf()" -toimintoa kirjoittaaksesi haluamasi sisällön tiedostoon. Lopuksi, muista sulkea tiedosto "fclose()" -toiminnolla.

```
#include <stdio.h>

int main() {
    // Luo tiedostonimike ja avaa tiedosto "example.txt" kirjoittamista varten
    FILE *tiedosto = fopen("example.txt", "w");
    
    // Kirjoita teksti tiedostoon käyttäen fprintf-toimintoa
    fprintf(tiedosto, "Tämä on esimerkki tekstistä, joka kirjoitetaan tiedostoon\n");
    
    // Sulje tiedosto
    fclose(tiedosto);
    
    return 0;
}

```

Tulostus tiedostoon olisi seuraavanlainen:

Tämä on esimerkki tekstistä, joka kirjoitetaan tiedostoon

## Syvällinen sukellus

Kun luot tekstityyliä, voit käyttää erilaisia muotoiluja ja erityismerkkejä tekstiisi. Voit myös lukea tiedostosta käyttäen "fscanf()" -toimintoa ja jopa poistaa tiedosto "remove()" -toiminnolla.

```
#include <stdio.h>

int main() {
    // Luo tiedostonimike ja avaa tiedosto "example.txt" lukemista varten
    FILE *tiedosto = fopen("example.txt", "r");
    
    // Määritä muuttuja "teksti" ja lue teksti tiedostosta
    char teksti[50];
    fscanf(tiedosto, "%s", teksti);
    
    // Tulosta teksti konsoliin
    printf("Tiedostosta luettu teksti on: %s\n", teksti);
    
    // Sulje tiedosto
    fclose(tiedosto);
    
    // Poista tiedosto "example.txt"
    remove("example.txt");
    
    return 0;
}

```

Tulostus olisi seuraavanlainen:

Tiedostosta luettu teksti on: Tämä

## Katso myös

- [fopen() käyttö C-kielen tekstityylissä](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [fprintf() käyttö C-kielen tekstityylissä](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Esimerkki tiedostojen lukemisesta ja kirjoittamisesta C-kielellä](https://www.programiz.com/c-programming/c-file-input-output)
- [Tutustu Markdown-kieleen, jota käytetään tämän blogin kirjoittamiseen](https://www.markdownguide.org/)