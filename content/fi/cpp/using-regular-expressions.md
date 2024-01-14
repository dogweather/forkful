---
title:    "C++: Säännöllisten lausekkeiden käyttö"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä C++ ohjelmoinnissa, ja ne tarjoavat tehokkaan tavan tarkistaa ja käsitellä merkkijonoja. Niitä voidaan käyttää esimerkiksi merkkijonon muotoilussa, hakemisessa ja korvaamisessa.

## Miten käyttää säännöllisiä lausekkeita

Säännöllisiä lausekkeita käytetään C++:ssa "regex" kirjastolla. Ensiksi, se täytyy sisällyttää ohjelmaan ```#include <regex>```. Sitten voimme käyttää säännöllisiä lausekkeita etsintään ja käsittelyyn.

Esimerkiksi voimme tarkistaa, onko merkkijono kokonaisluku regex-kirjasto avulla: 
```C++
#include <iostream>
#include <regex>

int main() {
  std::string s = "12345";
  
  // Luodaan regex objekti
  std::regex integer("^[0-9]+$");
  
  // Tarkistetaan, onko merkkijono kokonaisluku
  if (std::regex_match(s, integer)) {
    std::cout << "Merkkijono on kokonaisluku." << std::endl;
  } else {
    std::cout << "Merkkijono ei ole kokonaisluku." << std::endl;
  }
  return 0;
}
```
**Tuloste:**
```
Merkkijono on kokonaisluku.
```

## Syvällisempiä tietoja säännöllisten lausekkeiden käytöstä

Säännöllisissä lausekkeissa on laaja valikoima erilaisia toimintoja ja käyttötarkoituksia. Ne voivat sisältää sääntöjä, jotka määrittelevät merkkien ja merkkijonojen rakenteen ja muodon.

Säännöllinen lauseke `"^[0-9]+$"` käytettynä edellisessä esimerkissä tarkoittaa, että merkkijonossa voi olla vain numeromerkkejä ja sen pituus voi olla mikä tahansa. Voimme myös käyttää säännöllisiä lausekkeita tarkastelemaan esimerkiksi sähköpostiosoitteita, puhelinnumeroita tai jopa monimutkaisempia merkkijonoja.

## Katso myös

- [cppreference: regex](https://en.cppreference.com/w/cpp/regex) (englanniksi)
- [Tutoriaali säännöllisistä lausekkeista](https://www.tutorialspoint.com/cpp_standard_library/cpp_regular_expressions.htm) (englanniksi)
- [Säännölliset lausekkeet ja niiden sovellukset](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke) (suomeksi)