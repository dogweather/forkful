---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja miksi?
Testaus on koodin varmistamista sen toimimiseksi kuten pitää. Ohjelmoijat testaavat löytääkseen ja korjatakseen virheet nopeammin ja varmistaakseen, että koodimuutokset eivät riko aiemmin toiminutta.

## How to: - Miten:
```C
#include <assert.h>

// Yksinkertainen funktio testattavaksi
int summa(int a, int b) {
    return a + b;
}

// Testifunktio
void testaa_summa() {
    assert(summa(2, 2) == 4);
    assert(summa(-1, 1) == 0);
    // Lisää testejä tarvittaessa
}

int main() {
    testaa_summa();
    printf("Kaikki testit menivät läpi.\n");
    return 0;
}
```

Odotettu tulostus:
```
Kaikki testit menivät läpi.
```

## Deep Dive - Syväsukellus:
Historiallisesti C-kielessä ei ole ollut erillistä testikehystä. Ohjelmoijat ovat luoneet yksinkertaisia itsekirjoitettuja testejä, kuten `assert`-makroja. Vaihtoehtoja on kuten CUnit tai Unity testauskehykset. Näiden käyttö vaatii syvällisempää ymmärrystä ja operaatioita, kuten testien rekisteröintiä ja tulosten keräämistä.

## See Also - Katso Myös:
- CUnit: http://cunit.sourceforge.net/
- Unity Test Framework: https://www.throwtheswitch.org/unity
- C Standard Library (assert.h): https://en.cppreference.com/w/c/error/assert
