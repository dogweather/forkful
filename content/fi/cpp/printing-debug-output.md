---
title:                "C++: Tulosta vianmääritystieto"
simple_title:         "Tulosta vianmääritystieto"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi

Ohjelmointi voi olla monimutkaista ja virheiden löytäminen voi olla haastavaa. Tulosteiden tulostaminen koodin suorituksen aikana voi auttaa havaitsemaan mahdollisia ongelmia ja helpottaa virheiden jäljittämistä.

# Miten

Aloita lisäämällä tulosteiden tulostaminen haluamiisi kohtiin koodissa. Voit käyttää `cout`-komennolla tai `printf()`-funktiolla tulostamaan haluamasi tiedot konsoliin. Esimerkiksi:

```C++
cout << "Tämä on debug tuloste." << endl;
```

Näiden tulosteiden näkyminen konsolissa auttaa seuraamaan koodin suoritusta ja havaitsemaan mahdolliset virheet. Voit myös tulostaa muuttujien arvoja nähdäksesi niiden muutokset koodin suorituksen aikana.

```C++
int num = 5;
cout << "Muuttujan 'num' arvo on: " << num << endl;
```

Mikäli käytät `printf()`-funktiota, voit käyttää muotoilumerkkejä määrittämään tulostettavan muuttujan tyypin. Esimerkiksi:

```C++
int num = 5;
printf("Muuttujan 'num' arvo on: %d\n", num);
```

# Syvempi sukellus

Debug tulosteiden tulostaminen voi olla myös hyödyllistä selvittämään ohjelman toimintaa ja suoritusaikaa. Voit esimerkiksi käyttää `chrono`-kirjastoa mittaamaan ohjelman suoritusaikaa ja nähdäksesi, missä kohdassa ohjelma mahdollisesti hidastuu.

```C++
#include <chrono>
using namespace std::chrono;

// alustetaan kello
high_resolution_clock::time_point start = high_resolution_clock::now();

// koodia...

// lopetetaan kello ja tulostetaan kulunut aika
high_resolution_clock::time_point end = high_resolution_clock::now();
duration<double> time = duration_cast<duration<double>>(end - start);
cout << "Ohjelman suoritusaika: " << time.count() << " sekuntia." << endl;
```

Voit myös käyttää erilaisia debuggausohjelmia, kuten Visual Studio:n `Debug`-tilaa, joka mahdollistaa tulosteiden katselun ja ohjelman suorituksen pysäyttämisen halutuissa kohdissa.

# Katso myös

- [C++ Debugging Tutorial (Englanniksi)](https://www.guru99.com/c-plus-plus-debugging-tutorial.html)
- [Debugging Techniques in Visual Studio (Englanniksi)](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-techniques-in-visual-studio)
- [C++ Debugging Tips (Englanniksi)](https://www.educba.com/c-plus-plus-debugging-tips/)