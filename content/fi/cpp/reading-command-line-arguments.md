---
title:                "Komentoriviparametrien lukeminen"
html_title:           "C++: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat rakentaa monipuolisempia ja interaktiivisempia C++-ohjelmia, lukea komentoriviparametreja on hyödyllinen taito. Se antaa sinulle mahdollisuuden lukea syötteitä suoraan ohjelman suoritustilasta, mikä tekee ohjelmastasi joustavamman ja helpommin käytettävän.

## Kuinka

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    // argc kertoo kuinka monta parametria on annettu komentoriviltä
    // argv sisältää kaikki parametrit merkkijonoina
    // huomaa, että ensimmäinen parametri on aina itse ohjelman nimi (esim. ./ohjelma)
    std::cout << "Ohjelmaan annettiin " << argc - 1 << " parametriä." << std::endl;

    // käydään läpi kaikki parametrit ja tulostetaan ne yksi kerrallaan
    for (int i = 1; i < argc; i++) {
        std::cout << "Parametri " << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

Esimerkkituloste, kun suoritetaan ohjelma komentoriviltä seuraavasti: `./ohjelma hello world`

```
Ohjelmaan annettiin 2 parametriä.
Parametri 1: hello
Parametri 2: world
```

## Syväsukellus

Kun suoritat C++-ohjelmaa komentoriviltä, voit antaa sille haluamasi parametrit. Usein tätä ominaisuutta käytetään esimerkiksi tietokantayhteyksien määrittämiseen tai muokkaamaan ohjelman asetuksia. Komentoriviparametrien lukeminen tapahtuu `argc` ja `argv` muuttujien avulla. `argc` kertoo, kuinka monta parametria annettiin, ja `argv` sisältää nämä parametrit merkkijonoina.

Kun käsittelet komentoriviparametreja C++-koodissa, on tärkeää muistaa, että `argv` sisältää vain merkkijonot, eli sinun täytyy muuntaa ne tarvittaessa halutulle datatyyppiälle, kuten kokonaisluvuksi tai liukuluvuksi. Lisäksi kannattaa varmistaa, että käyttäjä antaa oikeat parametrit, esimerkiksi tietokantayhteyksissä tarkastellaan, onko annettu tietokantatiedosto olemassa ennen kuin yritetään muodostaa yhteys.

## Katso myös
- [C++ - Komentoriviparametrit](https://www.cplusplus.com/articles/DEN36Up4/)
- [C++ Tutorial - Command Line Arguments](https://www.studytonight.com/cpp/command-line-arguments-in-cpp.php)
- [How to read command line arguments in C++](https://stackoverflow.com/questions/3024197/how-to-read-command-line-arguments-in-c)