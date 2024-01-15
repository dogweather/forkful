---
title:                "Tiedoston lukeminen"
html_title:           "C: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi tekstitiedoston lukeminen C-ohjelmoinnin avulla on hyödyllistä. Esimerkiksi voit haluta analysoida suuren määrän tekstitiedostoja kokonaisuutena tai etsiä tiettyä tietoa tietyistä tiedostoista. Se voi myös olla hyödyllistä, jos haluat automaattisesti lukea tietoja ulkoisesta lähteestä osaksi ohjelmaasi.

## Miten tehdä

Käytä C:n "fopen" -funktiota avataksesi tiedosto ja tallentaaksesi se muuttujaan. Sitten voit käyttää "fscanf" -funktiota lukemaan tiedoston sisällön ja tallentamaan sen haluamaasi muuttujaan. Voit myös käyttää "fgets" -funktiota lukeaksesi rivin kerrallaan ja käsitelläksesi sen sitten haluamallasi tavalla. Alla on esimerkkejä kummastakin menetelmästä.

```C
FILE *file = fopen("tekstitiedosto.txt", "r"); // avaa tiedoston lukemista varten
char sana[20];
fscanf(file, "%s", sana); // tallentaa ensimmäisen sanan muuttujaan
printf("Ensimmäinen sana tiedostossa on: %s", sana); // tulostaa sanan "teksti"
```

```C
FILE *file = fopen("tekstitiedosto.txt", "r"); // avaa tiedoston lukemista varten
char rivi[100];
while (fgets(rivi, 100, file) != NULL) {
    // käsittele jokainen rivi tässä
}
```

## Tarkempi kuvaus

Tekstitiedoston lukeminen C-ohjelmoinnissa onnistuu "stdio.h" -kirjaston avulla. Tämän kirjaston avulla voit avata tekstitiedoston "fopen" -funktiolla ja käyttää sitten "fscanf" - tai "fgets" -funktioita lukeaksesi tiedoston sisältöä. Voit myös käyttää "fgetc" -funktiota, joka lukee tiedostosta yksittäisen merkin kerrallaan. Muista aina sulkea tiedosto "fclose" -funktiolla, kun olet lopettanut sen käytön.

## Katso myös

- [Fscanf C-kielessä](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)
- [Fgets C-kielessä](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)
- [Fgetc C-kielessä](https://www.tutorialspoint.com/c_standard_library/c_function_fgetc.htm)