---
title:    "C++: Merkkijono: Merkkijonon pituuden löytäminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi
Monet C++-ohjelmoijat haluavat tietää merkkijonon pituuden eri syistä, kuten esimerkiksi kun he haluavat varmistaa, että merkkijono mahtuu tiettyyn puskuriin, tai kun he haluavat laskea, kuinka monta merkkijonoa esiintyy tekstissä.

## Miten
Voit helposti löytää merkkijonon pituuden käyttämällä funktiota `length()` tai `size()`, jotka kuuluvat `string`-luokkaan. Seuraavassa on esimerkki:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string s = "Tämä on merkkijono";
    cout << "Pituus: " << s.length() << endl;
    cout << "Pituus: " << s.size() << endl;
    return 0;
}
```

**Tulostus:**
```
Pituus: 19
Pituus: 19
```

Voit myös käyttää `strlen()`-funktiota C-tyylisessä merkkijonossa, mutta se vaatii `cstring`-kirjaston sisällyttämistä. Seuraava esimerkki näyttää, kuinka voit käyttää sitä:

```C++
#include <iostream>
#include <cstring>
using namespace std;

int main() {
    char c[100] = "Tämä on merkkijono";
    cout << "Pituus: " << strlen(c) << endl;
    return 0;
}
```

**Tulostus:**
```
Pituus: 19
```

## Syventävä tarkastelu
Vaikka `length()`- ja `size()`-funktiot antavat saman tuloksen, ne toimivat hieman eri tavalla. `length()`-funktio palauttaa `size_t`-tyyppisen arvon ja `size()`-funktio palauttaa `size_t`-tyyppisen arvon, joten voit käyttää näitä arvoja erilaisissa matemaattisissa laskutoimituksissa. Lisäksi jos merkkijono on tyhjä, niin `length()` antaa arvoksi 0, kun taas `size()` antaa arvoksi 1.

On myös hyvä huomata, että jos käytät C-tyylistä merkkijonoa (kuten ensimmäisessä esimerkissä), niin `length()`- ja `size()`-funktiot eivät ole käytettävissä. Sinun täytyy siis muuntaa merkkijono `string`-luokan tyyppiseksi, jotta voit käyttää näitä funktioita.

## Katso myös
- [C plusplus Reference - string - length](http://www.cplusplus.com/reference/string/string/length/)
- [C plusplus Reference - string - size](http://www.cplusplus.com/reference/string/string/size/)
- [C plusplus Reference - string - c_str](http://www.cplusplus.com/reference/string/string/c_str/)