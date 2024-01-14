---
title:    "C++: Kirjoittaminen standardi virheeseen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi
Kuten monilla ohjelmointikielillä, myös C++:ssa on tapa tulostaa tekstiä standardivirheen kautta. Tässä blogikirjoituksessa käymme läpi, miksi tämä voi olla hyödyllistä ja miten se tehdään.

## Miten
```C++
#include <iostream>

int main() {
    std::cerr << "Tämä teksti tulostuu standardivirheen kautta" << std::endl;
    return 0;
}
```

Kun suoritamme tämän ohjelman, teksti "Tämä teksti tulostuu standardivirheen kautta" tulostuu terminaaliin tai konsoliin. Tämä on hyödyllistä esimerkiksi virheviestien tai debuggaustulosten tulostamisessa.

## Syvempi katsaus
Standardivirheen käyttäminen auttaa erottamaan tulosteen ja virheilmoitukset selkeämmin, sillä ne eivät sekoitu keskenään. Samalla se myös antaa mahdollisuuden ohjelman käyttäjälle tai kehittäjälle tarkastella virheitä ja poikkeuksia, jotka ovat tapahtuneet ohjelman suorituksen aikana.

Toinen hyödyllinen käyttötarkoitus standardivirheelle on ohjelman debuggaus. Tulostamalla tietoja suoraan standardivirheeseen, voi löytää nopeasti ja helposti ongelmakohtia ohjelmassa ja korjata ne.

## Katso myös
- [C++ opetusohjelma: Standardikirjastojen käyttö](https://www.cplusplus.com/doc/tutorial/standard_libs/)
- [Bash: Ohjaaminen standardiin ja standardivirheeseen](https://devhints.io/bash-redirect-output) 
- [Visual Studio Code: Debuggaaminen ja tulostaminen standardivirheeseen](https://code.visualstudio.com/docs/editor/debugging#_redirecting-output)