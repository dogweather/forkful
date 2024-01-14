---
title:    "C++: Stringin kirjoittaminen isolla alkukirjaimella"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää aikaa ja vaivaa muuttamalla merkkijonon kirjaimet isoiksi? Eikö se ole vain kosmeettinen muutos? Vaikka se saattaa tuntua pieneltä ja merkityksettömältä, on olemassa joitain tilanteita, joissa isoiksi kirjainiksi muuttaminen voi olla hyödyllistä. Esimerkiksi käsitellessäsi käyttäjän syöttämää dataa, saatat haluta varmistaa, että annetut merkkijonot ovat kaikki samassa muodossa, jotta myöhemmin ei ilmene virheitä. Näin ollen, capitalizing voi auttaa sinua välttämään potentiaalisia ongelmia koodin suorituksessa.

## Kuinka

Tässä on esimerkki siitä, kuinka voit muuttaa merkkijonon sisältämät kirjaimet isoiksi kirjaimiksi C++:ssa:

```C++
#include <iostream>
#include <string>
#include <locale>

std::string capitalize(std::string str) {
    // Luodaan olio, joka edustaa käytössä olevaa paikallistamista
    std::locale loc;
    
    // Käydään läpi merkkijonon jokainen kirjain
    for (size_t i = 0; i < str.length(); i++) {
        
        // Muutetaan kirjain isoksi kirjaimeksi käyttäen sijainnin mukana tulevaa paikallistamista
        str[i] = std::toupper(str[i], loc);
    }
    
    return str;
}

int main() {
    // Kysytään käyttäjältä merkkijono
    std::string input;
    std::cout << "Anna merkkijono: ";
    std::getline(std::cin, input);
    
    // Kutsutaan capitalize-funktiota ja tulostetaan palautettu merkkijono
    std::cout << "Merkkijonon tulisi nyt olla isoilla kirjaimilla: " << capitalize(input) << std::endl;
    
    return 0;
}
```

**Tulostus:**

```
Anna merkkijono: Hei, Maailma!
Merkkijonon tulisi nyt olla isoilla kirjaimilla: HEI, MAAILMA!
```

## Syvällinen sukellus

Kuten huomaat, tämä esimerkki käyttää `locale`-oliota paikallistamaan merkkijonon kirjaimet. Paikallistaminen määrittää, miten kulttuuri tai kieli käsittelee tiettyjä merkkejä, kuten aakkosia. Tämän avulla voit muuttaa kirjainkoot oikealla tavalla kulttuurista riippumatta.

Paikallistaminen on tärkeää myös siksi, että se voi mahdollistaa muiden kirjoitusjärjestelmien, kuten kyrillisen tai kiinan, oikeanlaisen käsittelyn. Jos et käytä paikallistamista, saatat kohdata ongelmia esimerkiksi muuttaessaan merkkijonoja eri kirjoitusjärjestelmistä.

## Katso myös

- ["[C++ String Case Conversion](https://www.tutorialspoint.com/cplusplus/cpp_strings_case_conversion.htm)" -sivusto](https://www.tutorialspoint.com/cplusplus/cpp_strings_case_conversion.htm)
- ["C++ Strings" -esimerkki Cppreference.com-sivustolla](https://en.cppreference.com/w/cpp/string/basic_string)
- ["Working With Strings in C++" -kirjoitus Guru99-sivustolla](https://www.guru99.com/cpp-string-processing.html)