---
title:                "Virheenjäljitystulostuksen tulostaminen"
html_title:           "C++: Virheenjäljitystulostuksen tulostaminen"
simple_title:         "Virheenjäljitystulostuksen tulostaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus koodin virheiden selvittäminen voi olla haastavaa ja tylsää. Onneksi on olemassa yksinkertainen tapa tehdä tästä prosessista hieman helpompaa - tulostamalla debug-tietoja.

## Miten

Voit tulostaa debug-tietoja koodissasi käyttämällä `cout`-funktiota. Alla on yksinkertainen esimerkki:

```C++
#include <iostream> 

int main() { 
    int x = 5; 
    std::cout << "Muuttujan x arvo on: " << x << std::endl; 
    return 0; 
}
```

Tämän koodin tulostus näyttäisi seuraavalta:

```
Muuttujan x arvo on: 5
```

Tässä esimerkissä käytettiin `cout`-funktion lisäksi myös muutamia `iostream`-kirjaston toimintoja. `std::cout <<` - osa tulostaa tekstiä konsoliin, `x` viittaa muuttujaan, ja `std::endl` lopettaa rivin ja siirtyy uudelle riville.

Voit myös tulostaa debug-tietoja muuttujien lisäksi myös funktioiden palauttamien arvojen kanssa. Alla on esimerkki:

```C++
#include <iostream> 

int addNumbers(int a, int b) { 
    return a + b; 
} 

int main() { 
    std::cout << "Lisäystuloksen arvo on: " << addNumbers(2, 3) << std::endl; 
    return 0; 
}
```

Tämän koodin tulostus näyttäisi seuraavalta:

```
Lisäystuloksen arvo on: 5
```

Käytännössä voit siis käyttää `cout`-funktiota tulostamaan melkein mitä tahansa haluat, ja nyt voit helposti tarkistaa, mitä tietoja koodisi käsittelee.

## Deep Dive

`cout`-funktion käyttäminen debug-tietojen tulostamiseen on hyödyllistä, mutta se ei ole ainoa tapa. C++:ssa on myös muita vaihtoehtoja, kuten `cerr`- ja `clog`-funktiot. Nämä ovat hieman erilaisia, mutta niiden perusidea on sama - tulostaa tekstiä konsoliin.

Usein on myös hyödyllistä lisätä tulostettavan tekstin lisäksi myös tietoa, mistä tietyn tulosteen kohdalla on kyse. Tämä on mahdollista käyttämällä `#define` -komentoa. Voit määrittää erilaisia `#define`-komentoja, jotka tekevät koodin lukemisesta ja ymmärtämisestä helpompaa.

## Näytä myös

- [cplusplus.com](http://www.cplusplus.com/reference/iostream/) - I/O kirjaston dokumentaatio.
- [W3Schools - C++ I/O](https://www.w3schools.com/cpp/cpp_io.asp) - Tietoja C++ I/O:sta.
- [cprogramming.com - Debugging: Kirjoittaminen konsoliin](https://www.cprogramming.com/debugging/writing-debugging-output.html) - Tietoa konsoliin kirjoittamisesta debug-tarkoituksiin.