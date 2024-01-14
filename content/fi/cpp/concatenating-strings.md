---
title:    "C++: Stringien yhdistäminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää merkkijonojen yhdistämistä (concatenation) ohjelmoinnissa? Yksinkertaisesti sanottuna, se mahdollistaa useiden merkkijonojen yhdistämisen yhdeksi merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi tulostettaessa tekstiä tai luotaessa dynaamisia viestejä.

## Miten

Merkkijonojen yhdistäminen C++:ssa on helppoa käyttämällä "+"-merkkiä. Alla olevassa esimerkissä käytämme kolmea merkkijonoa ja yhdistämme ne yhdeksi merkkijonoksi.

```C++
#include <iostream>

using namespace std;

int main()
{
    string hello = "Hei";
    string name = "Maailma";
    string greeting = hello + " " + name;
    
    cout << greeting << endl;
    
    return 0;
}

// Tulostaa: Hei Maailma
```

Kuten näette, voimme käyttää myös "+"-merkkiä yhdistääksemme merkkijonon ja muuttujan, kuten "hello + name" -rivillä.

## Syvällinen sukellus

Merkkijonojen yhdistämisessä kannattaa olla varovainen, että käytät oikeaa yhdistämismenetelmää. Jos käytät "+"-merkkiä liittämiseen (append), jokainen yhdistetty merkkijono täytyy kopioida uuteen merkkijonoon. Tämä voi aiheuttaa suorituskykyongelmia jos käsittelet useita suuria merkkijonoja.

Sen sijaan, voit käyttää "stringstream" -luokkaa, joka kerää merkkijonoja sisäisesti bufferiin ennen niiden yhdistämistä. Alla olevassa esimerkissä yhdistämme useita merkkijonoja käyttäen "stringstream"iä:

```C++
#include <iostream>
#include <sstream>

using namespace std;

int main()
{
    stringstream ss;
    ss << "Tämä " << "on " << "yhdistetty " << "merkkijono";
    string result = ss.str();
    
    cout << result << endl;
    
    return 0;
}

// Tulostaa: Tämä on yhdistetty merkkijono
```

Lisätietoa erilaisista merkkijonojen yhdistämismenetelmistä löydät esimerkiksi täältä: [https://www.geeksforgeeks.org/string-manipulation-in-c-2/](https://www.geeksforgeeks.org/string-manipulation-in-c-2/)

## Katso myös

- [https://www.programiz.com/cpp-programming/string](https://www.programiz.com/cpp-programming/string)
- [https://www.tutorialspoint.com/cplusplus/cpp_strings.htm](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [https://www.learncpp.com/cpp-tutorial/78-working-with-strings/](https://www.learncpp.com/cpp-tutorial/78-working-with-strings/)