---
title:                "C: Komentoriviparametrien lukeminen"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Miksi:

Monet C-ohjelmoijat saattavat haluta lukea komentoriviargumentteja ohjelmaansa, jotta he voivat muokata sen toimintoja ja parametreja ilman koodin muokkaamista.

### Miten:

Komentoriviargumenttien lukeminen on tärkeä taito C-ohjelmoinnissa, ja se onnistuu muutamalla yksinkertaisella askeleella.

1. Esimerkiksi, käytä main-funktiota ja sen parametria argc.

```
#mukana <stdio.h>

int main(int argc, char *argv[]) {

    //tulostaa yhteensä komentoriviargumenttien määrän
    printf("Yhteensä %d argumenttia annettu.\n", argc);

    //tulostaa kaikki argumentit
    for (int i = 0; i < argc; i++) {
        printf("%s\n", argv[i]);
    }

    palauta 0;
}
```

2. Käännä ja aja ohjelma komentorivillä antamalla argumentteja, esimerkiksi:

```
./ohjelma argumentti1 argumentti2 argumentti3
```

Nyt voit havaita, että printf-käsky tulostaa argumenttien määrän ja kaikki argumentit, joita syötät ohjelman nimeä seuraavan jälkeen.

### Syvennys:

Komentoriviargumenttien lukeminen on hyödyllistä esimerkiksi silloin, kun haluat käsitellä ohjelmasi tietoja dynaamisesti. Voit esimerkiksi vaihtaa tiettyjä toimintoja ohjelmassasi antamalla erilaisia argumentteja.

Lisäksi voit myös käyttää strcmp-funktiota vertailemaan argumentteja ja reagoida sen mukaan. Tämä avaa mahdollisuuksia kehittää joustavampia ja monipuolisempia ohjelmia.

### Katso myös:

1. [C-kielen virallinen dokumentaatio](https://en.cppreference.com/w/c/language/main_function)
2. [Codecademy C-opas](https://www.codecademy.com/learn/learn-c)
3. [C-ohjelmoinnin 10 tärkeintä käsitettä](https://medium.com/launch-school/c-ohjelmoinnin-10-t%C3%A4rkeint%C3%A4-k%C3%A4sitett%C3%A4-d99ef5117246)