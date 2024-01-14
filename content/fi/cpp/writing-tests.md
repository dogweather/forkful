---
title:                "C++: Testien kirjoittaminen"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi: Miksi kirjoittaa testeja?

Kirjoittaminen testeja on tärkeä osa ohjelmistokehitystä, koska se varmistaa koodin toimivuuden ja vähentää mahdollisia virheitä. Testien avulla voit myös helpommin tunnistaa ja korjata mahdollisia ongelmia koodissasi ennen kuin ne aiheuttavat vakavampia ongelmia.

## Miten: Esimerkkejä C++ koodin kera

Testien kirjoittaminen C++:ssa on helppoa, kun tiedät mitä teet. Alla on muutamia esimerkkejä testein kirjoittamisesta koodiblokkien avulla.

```C++
#include <iostream>
#include "functions.h"

using namespace std;

int main() {
    // Luodaan testiluokka
    class Test {
        private:
            int a, b;

        public:
            // Luodaan konstruktori
            Test(int x, int y) {
                a = x;
                b = y;
            }

            // Testataan funktiota sum
            int testSum() {
                return sum(a, b);
            }
    };

    // Määritetään testiluokan objekti
    Test myTest(3, 7);

    // Ajetaan testi ja tulostetaan tulos
    cout << "Tulos: " << myTest.testSum();
}

// Output:
// Tulos: 10
```

Edellä olevassa esimerkissä luomme testiluokan ja ajamme testin funktiolle "sum", joka lisää kaksi numeroa yhteen. Testi tulostaa oikean tuloksen, mikä tarkoittaa että funktiomme toimii odotetusti.

## Syväsukellus: Syvempää tietoa testeistä

Testien kirjoittaminen ei ainoastaan auta sinua havaitsemaan ja korjaamaan mahdollisia virheitä, vaan se myös auttaa sinua ymmärtämään koodiasi paremmin. Kirjoittamalla testejä, sinun täytyy ajatella koodiasi eri näkökulmista ja tarkkailla sen toimintaa erilaisilla syötteillä.

Testit myös auttavat sinua välttämään koodin muuttumista liian monimutkaiseksi. Kun tiedät tarkalleen mitä koodisi pitäisi tehdä, ei ole niin helppoa kirjoittaa ylimääräistä koodia tai tehdä tarpeettomia muutoksia.

## Katso myös
- [C++ testaamisen aloittelijan opas](https://www.softwaretestinghelp.com/cpp-unit-testing-beginners-guide/)
- [Miksi testaaminen on tärkeää ohjelmistokehityksessä](https://www.agilealliance.org/glossary/tdd/)
- [C++:n testaaminen käyttäen Google Test frameworkia](https://github.com/google/googletest)