---
title:    "C++: Tulostaminen vianmääritystulosteita"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Ohjelmoinnin aikana voi olla tarpeen tulostaa debug-tietoa ohjelman suorituksen aikana. Tämä auttaa kehittäjiä ymmärtämään ohjelman toimintaa ja etsimään mahdollisia virheitä. 

## Miten

Debug-tulosteen käyttäminen on helppoa C++:ssa. Yksinkertaisimmillaan voit käyttää ```std::cout``` -funktiota tulostamaan haluamasi tiedon konsoliin. Esimerkiksi:

```C++
#include <iostream>

int main() {
    int a = 5;
    std::cout << "Muuttujan a arvo on: " << a << std::endl;
    return 0;
}
```

Tämä tulostaisi konsoliin "Muuttujan a arvo on: 5". Voit myös käyttää ```<<``` -operaattoria tulostaessasi monta muuttujaa samassa rivissä.

## Syventävä sukellus

Debug-tulosteen käyttö ei rajoitu pelkästään perustiedon tulostamiseen. Voit käyttää myös erilaisia muotoilumerkkejä saadaksesi haluamasi tulosteen muodon. Esimerkiksi:

```C++
#include <iostream>

int main() {
    double num = 3.14;
    std::cout << "Merkkijonon pituus: " << std::fixed << std::setprecision(2) << num << std::endl;
    return 0;
}
```

Tämä tulostaisi konsoliin "3.14". Lisäksi voit käyttää erilaisia ehtolauseita ja silmukoita voidaksesi tulostaa haluamasi tiedon useita kertoja ja erilaisilla ehdoilla.

## Katso myös

- [Tutorial: Debug-tulosteen käyttö C++:ssa](https://www.tutorialspoint.com/cplusplus-program-for-debugging)
- [C++ Reference: std::cout](https://en.cppreference.com/w/cpp/io/cout)
- [C++:n debuggaus vinkkejä](https://medium.com/@ma_ji/cpp-debugging-hacks-b1f283fa85a)