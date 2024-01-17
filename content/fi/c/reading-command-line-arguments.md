---
title:                "Lukemalla komentoriviparametreja"
html_title:           "C: Lukemalla komentoriviparametreja"
simple_title:         "Lukemalla komentoriviparametreja"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Komentoriviparametrien lukeminen tarkoittaa ohjelman argumenttien lukemista komentoriviltä. Tämä on tärkeää, jotta voidaan antaa ohjelmalle tarvittavat syötteet sen suorittamiseen. 

Ohjelmoijat lukevat komentoriviparametreja, jotta he voivat mukauttaa ohjelmiaan käyttäjän antamilla arvoilla. Tämä tekee ohjelmista joustavampia ja käyttäjäystävällisempiä.

## Miten tehdään?

Voit lukea komentoriviparametreja hyödyntämällä **argc** ja **argv** -muuttujia, joiden avulla pääset käsiksi kaikkiin komentorivillä annettuihin argumentteihin. Käytämme **argc**-muuttujaa saadaksemme tiedon siitä, kuinka monta komentoriviparametria on annettu, ja **argv**-muuttujaa saadaksemme itse parametrien arvot.

```C
int main(int argc, char* argv[]) {
    // Käytä argc:ta parametrien lukumäärän tarkistamiseen
    if (argc < 2) {
        printf("Komentoriviparametreja ei annettu!\n");
        return 0;
    }
    
    // Käytetään argv-muuttujaa tarkistamaan tiettyä parametria (esim. "tulosta")
    for (int i = 0; i < argc; i++) {
        if (strcmp(argv[i], "tulosta") == 0) {
            printf("Parametri 'tulosta' annettu!\n");
        }
    }
    return 0;
}
```

**Esimerkki syötteestä ja tulosteesta:**

```
$ ./ohjelma tulosta lisää tuloste
Parametri 'tulosta' annettu!
```

## Syvällisemmin

Alunperin komentoriviparametreja käytettiin vain komentokehotteisiin, jotta voitaisiin helposti antaa tietoa ohjelmalle sen suorittamista varten. Nykyään käyttö on laajentunut muun muassa graafisten käyttöliittymien yhteydessä.

Myös muita tapoja antaa ohjelmalle syötteitä on olemassa, kuten lataamalla ne tiedostoista tai käyttämällä ympäristömuuttujia. Jokaisella vaihtoehdolla on omat etunsa ja haittansa.

Komentoriviparametrien lukeminen perustuu ANSI C -standardiin, joten se on mahdollista kaikilla C-kielellä kirjoitetuilla ohjelmilla. Kuitenkin jokaisella käyttöjärjestelmällä voi olla omat toteutustapansa komentorivin käsittelyyn ja parametrien välittämiseen ohjelmalle.

## Katso myös

[Lukemattomat komentoriviargumentit](https://linux.die.net/man/3/getopt) - C-kirjasto lukemiseen argumentteja komentoriviltä Linux-käyttöjärjestelmässä

[Komentoriviparametrien lukeminen Visual Studiolla](https://docs.microsoft.com/en-us/cpp/cpp/how-to-parse-command-line-arguments) - Ohjeita ja esimerkkejä lukemiseen komentoriviargumentteja Visual Studiolla.