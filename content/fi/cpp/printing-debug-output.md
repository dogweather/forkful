---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tulosteen virheenkorjauksen tulostaminen on ohjelman suorittamisen aikana tapahtuvaa informaation tulostamista, usein konsolille. Se auttaa ohjelmoijia ymmärtämään ja korjaamaan ohjelman toiminnan virheitä tai virheitä.

## Miten tehdään:

Käytämme C++:n sisäänrakennettua `cout`-funktiota. Tässä on perusotsikotiedosto ja kaksi erilaista tapaa esittää debug-tietoja.

```C++
#include <iostream>

int main() {
    int val = 5;
    std::cout << "Debug: Value is " << val << "\n";
    return 0;
}
```
Ja sample-ohjelman antama output on `Debug: Value is 5`.

Tai vaihtoehtoisesti voit käyttää `cerr`-funktiota:

```C++
#include <iostream>

int main() {
    int val = 5;
    std::cerr << "Debug: Value is " << val << "\n";
    return 0;
}
```
Johtuen `cerr`:n erilaisesta käsittelystä, tämä tuottaa saman tuloksen, mutta tietovirta tulee eri lähteestä.

## Syvempi katsaus

Historiallisessa kontekstissa, ohjelmoijat ovat käyttäneet debug-tulosteita lähes yhtä kauan kuin ohjelmointikieliä on ollut olemassa. Se on yksinkertainen mutta tehokas työkalu. 

C++:ssa on erilaisia vaihtoehtoja: voit käyttää `cout` standardia tiedon ulostuloa tai `cerr` virheen ulostulolokiin. Tämä on erityisen hyödyllistä, jos ohjelmasi ohjaa kaksi erillistä ulostulovirtaa.

Toteutuksen yksityiskohdissa, `cout` tulostaa lopullisen tuotannon, kun taas `cerr` tulostaa ohjelman virhetulostuksiin. `cerr` on tärkeä, kun halutaan tulostaa virheet heti, koska se ei joudu mukaan puskurointiin kuten `cout`.

## Katso myös

C++ virheenkäsittelyn yleiskuvaus: http://www.cplusplus.com/doc/tutorial/exceptions/
C++ IOstream Library: http://www.cplusplus.com/reference/iostream/ 
C++ Debugging with Visual Studio: https://docs.microsoft.com/en-us/visualstudio/debugger/how-to-use-the-debugger?view=vs-2019