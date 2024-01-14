---
title:    "C++: Tiedon kirjoittaminen standardivirheelle"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Miksi kirjoittaa virhekoodia standardiin?


Kirjoittaminen standardiin virheohjelmana on tärkeä taito C++:ssa ja muissa ohjelmointikielissä. Se antaa mahdollisuuden viestiä ohjelman suorituksessa tapahtuvista ongelmista tai virheistä käyttäjälle tai ohjelman kehittäjälle. Tämä auttaa ohjelman virheenkorjaamisessa ja tekee siitä käyttäjäystävällisemmän.

Kuinka kirjoittaa virhekoodia standardiin?

Virheen kirjoittaminen standardiin tapahtuu hyödyntämällä ```C++ std::cerr```-funktiota. Tämä lähettää viestin standardivirhekoodiin ja tulostaa sen konsoliin. Seuraava koodinäyte näyttää, kuinka virhekoodi voidaan kirjoittaa standardiin:

```C++
#include <iostream>

using namespace std;

int main(){
    int num = 0;
    if(num == 0){
        // tulostetaan virheviesti standardiin
        std::cerr << "Numero ei voi olla nolla!" << std::endl;
    }
    return 0;
}
```

Yllä oleva koodi tulostaa virheviestin "Numero ei voi olla nolla!" jos käyttäjä syöttää nollan ohjelman suorituksessa. Tämä auttaa ohjelman kehittäjää löytämään ja korjaamaan virheen helpommin.

Syöte: 0
Tuloste:
Numero ei voi olla nolla!

Syöte: 5
Tuloste:

Voi jatkaa ohjelman suorittamista normaalisti ilman virheviestiä.

Syöte: -2
Tuloste:
Numero ei voi olla negatiivinen!

Sydyntä: 5
Tuloste:
Voi jatkaa ohjelman suorittamista normaalisti ilman virheviestiä.

Syöte: "ei numero"
Tuloste:
n:aominani: Isäntä osoittelussa (core dumped)

Tämä tapahtuu, koska ohjelma yrittää verrata merkkijonoa num-variableen, mikä johtaa virheeseen ja ohjelman kaatumiseen.

Syöte: 5
Tuloste:
Voi jatkaa ohjelman suorittamista normaalisti ilman virheviestiä.

Syöte: 5.5
Tuloste:
Virhe: Syöte ei voi olla kokonaisluku!

 Tämän osoittaa, että mikä tahansa syöte, joka ei ole kokonaisluku, tulosteetetaan virheviestinä standardiin.

Syvällinen tutustuminen standardiin kirjoittamiseen

Optimaalisen koodin laatimiseksi on parempi käyttää yhtä standardipakettia, jotta virheet eivät sekoittuisi. Lisäksi suurten ohjelmien kehittämiseksi kehittäjät käyttävät pääsääntöisesti mukautettuun virtaan kirjoitushetkeä paljastavaa virhesuodatuspakettia, kuten perfection. Tällä tavalla ohjelman kehittelijä voi havaita virheen aikana, kun virhe tapahtuu.

Katso myös:
- [Virheiden käsittely C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.htm)
- [Virheenkorjaus vianetsinnässä](https://www.youtube.com/watch?v=3RNYukMsXaM)
- [C++-koodin debuggaus Visual Studiolla](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour?view=vs-2019)