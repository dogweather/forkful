---
title:    "C: Lukeminen komentoriviparametreja"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan blogipostausta komentorivin argumenttien lukemisesta. Tässä artikkelissa käsittelemme, miksi on tärkeää osata lukea komentorivin argumentteja ja miten se voidaan tehdä C-ohjelmoinnissa. Jos olet kiinnostunut oppimaan lisää tästä tärkeästä aiheesta, jatka lukemista!

## Miten

Komentorivin argumentit ovat käyttäjän antamia tietoja ohjelmalle sen ajamisen yhteydessä. Ne ovat tärkeitä, koska ne antavat ohjelmalle tietoa siitä, miten sitä tulee käyttää ja mitä toimintoja suorittaa. Tässä on yksinkertainen esimerkki, miten voidaan lukea komentorivin argumentteja C-kielellä:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Sinulla on %d argumenttia.\n", argc);
  for (int i = 0; i < argc; i++) {
    printf("Argumentti %d: %s\n", i, argv[i]);
  }

  return 0;
}
```

Esimerkki koodi luo yksinkertaisen ohjelman, joka tulostaa kaikki komentoriviltä saadut argumentit. Jos suoritat ohjelman seuraavasti: ```./ohjelma hello world```, saat seuraavan tulosteen:

```
Sinulla on 3 argumenttia.
Argumentti 0: ./ohjelma
Argumentti 1: hello
Argumentti 2: world
```

Nyt voit nähdä, miten koodi käsittelee argumentteja ja tulostaa ne käyttäjälle. Tämä on vain yksinkertainen esimerkki, mutta voit alkaa kehittämään sitä ja lisätä siihen toiminnallisuuksia ohjelmointitaitosi mukaan.

## Syvempää tietoa

Kun suoritat ohjelmia komentoriviltä, saatat huomata, että jotkin argumentit ovat valinnaisia ja jotkin ovat pakollisia. Tämä tarkoittaa, että ohjelma ei välttämättä toimi oikein, jos et anna sille tiettyjä argumentteja. Tämä on tärkeää muistaa, kun suunnittelet ja kehität ohjelmia.

On myös tärkeää, että osaat käsitellä komentorivin argumentteja oikein. Tässä on joitakin hyödyllisiä resursseja, jotka auttavat sinua syventymään aiheeseen:

- [Argumenttien lukeminen C-ohjelmassa](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Ohjelmointitermit: Komentorivin argumentit](https://www.summarecon.com/en/glossary/command-line-arguments/)
- [C: Komentorivin argumentit](https://www.codingunit.com/c-program-to-accept-command-line-arguments)

## Katso myös

Jos haluat oppia lisää C-ohjelmoinnista ja muista aiheista, tarkista seuraavat resurssit:

- [C-ohjelmoinnin perusteet](https://www.sololearn.com/Course/C/)
- [Parhaat käytännöt C-ohjelmoinnissa](https://www.thegeekstuff.com/2011/12/c-programming-language-tutorial/)
- [Ohjelmointitermit: C-ohjelmointi](https://www.summarecon.com/en/glossary/c-programming/)