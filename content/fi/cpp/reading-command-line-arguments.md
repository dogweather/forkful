---
title:    "C++: Asetusten lukeminen komentoriviltä"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miksi käyttää komentoriviparametrejä ohjelmoinnissa

Komentoriviparametrit ovat tärkeitä osia ohjelmoinnissa, sillä ne mahdollistavat ohjelman käyttäjän vuorovaikutuksen ja antavat mahdollisuuden ohjelman käyttämiseen eri tavoilla. Niiden avulla voit ohjelmoida monipuolisempia ja käytännöllisempiä sovelluksia.

## Miten käyttää komentoriviparametrejä

Esimerkiksi, jos haluat ohjelmasi ottamaan vastaan käyttäjän antaman luvun ja kertomaan sen kahdella, voit käyttää komentoriviparametrejä tämän saavuttamiseksi. Alla olevassa esimerkissä luodaan yksinkertainen C++ ohjelma, joka ottaa vastaan yhden komentoriviparametrin ja tulostaa sen kaksinkertaisena.

```C++ 
#include <iostream> 

using namespace std;

int main(int argc, char *argv[])
{
    // tarkistetaan, että ohjelmaan on annettu yksi parametri
    if(argc != 2)
    {
        cout << "Anna luku komentoriviparametrina!" << endl;
        return 1; 
    }

    // muutetaan parametri merkkijonosta kokonaisluvuksi
    int luku = atoi(argv[1]);

    // tulostetaan luku kaksinkertaisena
    cout << "Annoit luvun " << luku << ", sen kaksinkertainen on " << luku * 2 << endl;

    return 0;
}
```

Ajamalla ohjelma komentorivillä antamalla parametri, esimerkiksi `./kaksinkertainen 5`, ohjelma tulostaa `Annoit luvun 5, sen kaksinkertainen on 10`.

## Syvällisempi sukellus komentoriviparametreihin

Komentoriviparametrit välitetään ohjelmalle käynnistettäessä ja ne tallennetaan `argv` (argument vector) taulukkoon ja niiden määrä tallennetaan muuttujaan `argc` (argument count). Taulukon ensimmäinen alkio sisältää aina ohjelman nimen, joten varsinaiset parametrit sijaitsevat sijainneissa 1, 2, jne. Taulukon alkiot ovat aina merkkijonoja, joten tarvittaessa ne täytyy muuttaa halutuiksi tietotyypeiksi, kuten esimerkissämme teimme käyttämällä `atoi()` funktiota.

On myös hyvä mainita, että ohjelmat voivat ottaa vastaan useita komentoriviparametrejä ja että niitä voi käyttää monella eri tapaa riippuen ohjelman tarpeista. Esimerkiksi, jos ohjelmasi on graafinen käyttöliittymä, voit käyttää komentoriviparametreja määrittämään, millä tavalla ohjelma käynnistetään, kuten pienenä ikkunana tai koko näytöllä.

# Katso myös

- [C++ Foundation - Komentoriviparametrit](https://www.includehelp.com/cpp-tutorial/command-line-arguments-in-cpp.aspx)
- [GeeksforGeeks - Komentoriviparametrit C++:ssa](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)