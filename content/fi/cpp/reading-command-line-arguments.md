---
title:                "C++: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviargumenttien lukeminen on tärkeä osa C++ ohjelmointia, sillä se mahdollistaa käyttäjien syötteiden käsittelyn ja erilaisten toimintojen suorittamisen ohjelmassa. Tässä blogikirjoituksessa tutustumme tarkemmin komentoriviargumenttien lukemiseen C++ -koodissa ja opimme, miten voimme hyödyntää tätä taitoa ohjelmoinnissamme.

## Miten

Komentoriviargumenttien lukeminen onnistuu C++:n standardikirjaston `argc` ja `argv` muuttujien avulla. `argc` sisältää arvonaan komentoriviargumenttien lukumäärän ja `argv` on merkkijonojen taulukko, joka sisältää itse komentoriviargumentit. Alla on esimerkkikoodi, joka tulostaa kaikki komentoriviargumentit yksitellen:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
  for (int i = 0; i < argc; i++) {
    std::cout << argv[i] << std::endl;
  }
  return 0;
}
```

Esimerkkituloste, kun ohjelmaa suoritetaan komennolla `./my_program hello world`:

```
./my_program
hello
world
```

## Syvällisempi tarkastelu

Komentoriviargumenttien lukeminen ei rajoitu vain yksittäisten syötteiden tulostamiseen, vaan niitä voi myös käyttää esimerkiksi ehtolauseiden tai silmukoiden ehtoina. Lisäksi `argc` ja `argv` muuttujia voidaan hyödyntää myös erilaisten virheiden käsittelyssä. Esimerkiksi jos tiettyjä argumentteja ei ole määritelty tai niiden arvot ovat virheellisiä, voidaan ohjelma lopettaa virheilmoituksella.

## Katso myös

- [C++ Standardikirjaston dokumentaatio](https://en.cppreference.com/w/cpp/language/main_function)
- [C++ Kirja: Komentoriviargumenttien lukeminen](https://en.wikibooks.org/wiki/C%2B%2B_Programming/Code/Standard_C_Library/Functions/argc_and_argv)