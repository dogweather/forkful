---
title:                "C++: Komentoriviparametrien lukeminen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi lukijamme saattavat haluta oppia kuinka lukea komentoriviparametrejä C++-ohjelmoinnissa. Saatat tarvita tätä taitoa, jotta voit kirjoittaa ohjelmia, jotka ovat yhteensopivia muiden komentorivipohjaisten työkalujen kanssa, tai ehkä haluat tehdä ohjelmastasi käyttäjäystävällisemmän antamalla käyttäjien syöttää parametreja suoraan ohjelmalle.

## Kuinka

Aloita luomalla pääfunktio, joka ottaa parametreiksi "argc" ja "argv". Tämä pääfunktio olioina voidaan käyttää lukemaan komentoriviparametreja. Tässä on yksinkertainen esimerkki:

```C++
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    // tulosta komentoriviparametrien lukumäärä
    cout << "Komentoriviparametrien lukumäärä: " << argc << endl;

    // tulosta kaikki parametrit 
    for (int i = 0; i < argc; ++i) {
        cout << "Parametri " << i << ": " << argv[i] << endl;
    }

    return 0;
}
```

Tässä esimerkissä käytämme "cout" -toimintoa tulostamaan lukumäärän ja kaikki parametrit. Voit käyttää myös muita C++:n toimintoja, kuten "string" -luokkaa, käsittelläksesi parametrit haluamallasi tavalla.

Kun suoritat tämän ohjelman komentoriviltä antamalla sille muutaman parametrin, esimerkiksi "ohjelmamme parametri1 parametri2", saamme seuraavan tuloksen:

```
Komentoriviparametrien lukumäärä: 3
Parametri 0: ohjelmamme
Parametri 1: parametri1
Parametri 2: parametri2
```

## Syvempi sukellus

Kun ohjelmasi on saanut komentoriviparametrit, voit käsitellä niitä ja tehdä ohjelmastasi vieläkin monipuolisemman esimerkiksi käyttämällä "if"-lausekkeita tai luokkia. Voit myös käyttää "stringstream" -luokkaa muuttamaan parametrit eri tyypeiksi, kuten "int" tai "double". Muista myös käsitellä mahdollisia virheilmoituksia, jos käyttäjä ei anna oikeaa määrää parametreja tai antaa virheellisen parametrin.

## Katso myös

- [C++:n "main"-funktio](https://www.cplusplus.com/articles/4z18T05o/)
- [string-luokka](https://www.cplusplus.com/reference/string/string/)
- [stringstream-luokka](https://www.cplusplus.com/reference/sstream/stringstream/)