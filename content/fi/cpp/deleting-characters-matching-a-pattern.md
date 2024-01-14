---
title:    "C++: Mallia vastaavien merkkien poistaminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi poistaa merkkejä, jotka vastaavat kaavaa?

Poistaminen merkkejä, jotka vastaavat tiettyä kaavaa, voi olla hyödyllistä, kun käsittelet suuria määriä tietoa ja haluat helposti puhdistaa tiedostot tai merkkijonot ei-toivotuista merkeistä. Tämä voi myös säästää aikaa ja vaivaa manuaalisen poistamisen sijaan.

## Miten tehdä se?

Tässä esimerkissä käytämme C++:aa poistaaksemme kaikki välimerkit annetusta merkkijonosta.

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string teksti = "Tämä on, teksti!"
    string puhdistettu_teksti = "";
    
    for (int i = 0; i < teksti.length(); i++) {
        char c = teksti[i];
        // Poistetaan kaikki välimerkit
        if(!ispunct(c)) {
            puhdistettu_teksti += c;
        }
    }
    cout << puhdistettu_teksti; // Tulostaa "Tämä on teksti!"
    
    return 0;
}
```

## Syväsukellus

Vaikka tässä esimerkissä käytämme vain yksinkertaista merkkijonon puhdistamista, voit soveltaa samaa periaatetta monimutkaisempien kaavojen poistamiseen. Voit myös käyttää säännöllisiä lausekkeita (regular expressions) helpottamaan prosessia, jos et halua kirjoittaa manuaalisesti jokaiselle poistettavalle merkille omaa ehtoa.

## Katso myös

- [C++ merkkijonon puhdistamisesta](https://stackoverflow.com/questions/19138983/how-to-remove-punctuation-from-a-string-in-c)
- [Säännölliset lausekkeet C++:ssa](http://www.cplusplus.com/reference/regex/)
- [Miten käyttää merkkijonoja ja kirjastoja C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)