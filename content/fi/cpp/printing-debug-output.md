---
title:                "C++: Virheenkorjaustulostuksen tulostaminen"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

Miksi: Miksi tulostaa debuggaustulosteita?

Debuggaus on tärkeä osa ohjelmoinnin prosessia, ja tulosteiden tarkasteleminen on hyvä tapa löytää ja korjata ohjelmissa olevia virheitä. Tulosteiden avulla voit seurata ohjelman suoritusta ja ymmärtää paremmin sen toimintaa. Tämä on erityisen tärkeää silloin, kun ohjelma ei toimi odotetusti tai kun haluat varmistaa, että tietty koodinpätkä toimii oikein.

Kuinka: Esimerkkejä debuggaustulosteiden tulostamisesta

Debuggaustulosteen tulostaminen C++: ssa on helppoa. Voit käyttää "cout" -funktiota ja sen sisään antaa haluamasi muuttujan tai viestin. Voit myös lisätä tulosteisiin muuttujien arvoja helpottaaksesi virheiden löytämistä.

```C++
#include <iostream>
using namespace std;

int main() {
    int x = 5;
    cout << "x- muuttujan arvo on " << x << endl;
    cout << "Ihan mitä vain haluat tulostaa debuggaustarkoitukseen" << endl;
    return 0;
}
```

Tuloste: 
```
x- muuttujan arvo on 5
Ihan mitä vain haluat tulostaa debuggaustarkoitukseen
```

Voit myös käyttää "cerr" -funktiota, joka tulostaa viestin virhe virtaan, jolloin voit nähdä sen esimerkiksi käyttöjärjestelmän konsolissa.

```C++
#include <iostream>
using namespace std;

int main() {
    int x = 5;
    cerr << "Debuggaustuloste virhevirtaan" << endl;
    cerr << "x- muuttujan arvo on " << x << endl;
    return 0;
}
```

Tuloste: 
```
Debuggaustuloste virhevirtaan
x- muuttujan arvo on 5
```

Syntaksin värikoodeilla voit myös helpottaa tulosteiden lukemista ja erottaa ne selkeämmin muusta koodista.

Deep Dive: Syvällisempiä tietoja debuggaustulosteiden tulostamisesta

Yllä mainittujen esimerkkien lisäksi on olemassa monia muita tapoja tulostaa debuggaustulosteita C++: ssa. Voit esimerkiksi käyttää "assert" -funktiota, joka tulostaa automaattisesti viestin, jos määritetty ehto ei täyty.

```C++
#include <iostream>
#include <cassert>
using namespace std;

int main() {
    int x = 5;
    assert(x == 10); // Tämä tulostaa virheviestin, koska x ei olekaan 10
    cout << "Tämä viesti ei tulostu" << endl;
    return 0;
}
```

Tuloste: 
```
Assertion failed
```

Voit myös hyödyntää C++: n "debug" -tilaa, joka on tarkoitettu nimenomaan debuggaustulosteiden tulostamiseen. Tämä tila on käytössä vain debuggauksen aikana, joten varsinaiseen tuotantoon se ei vaikuta.

```
#include <iostream>
using namespace std;

int main() {
    int x = 5;
    #ifdef DEBUG // Tämä ehto pätee, jos olet debuggauksessa
        cout << "x- muuttujan arvo on " << x << endl;
    #endif
    cout << "Tämä viesti tulostuu aina" << endl;
    return 0;
}
```

Tuloste debuggauksessa:
```
x- muuttujan arvo on 5
Tämä viesti tulostuu aina
```

Katso myös:
- [C++ Debuggausopas](https://www.tutorialspoint.com