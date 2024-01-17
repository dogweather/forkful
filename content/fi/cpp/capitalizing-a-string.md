---
title:                "Merkkijonon muuttaminen isointa kirjainta käyttäväksi"
html_title:           "C++: Merkkijonon muuttaminen isointa kirjainta käyttäväksi"
simple_title:         "Merkkijonon muuttaminen isointa kirjainta käyttäväksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Capitalizeeraus on prosessi, jossa merkkijonon ensimmäinen kirjain vaihdetaan isoon kirjaimeen. Tämä tehdään yleensä siistimään syötteenä olevaa tietoa tai parantamaan luettavuutta. Koodaajat käyttävät tätä tekniikkaa esimerkiksi kirjoittaessaan otsikoita tai luetteloidessaan tietoja aakkosjärjestyksessä.

## Miten:
### Esimerkki 1:
```C++
#include <iostream>
#include <string>
#include <cctype>
using namespace std;

int main() {
    string s = "tämä on testi";
    s[0] = toupper(s[0]);
    cout << s << '\n';

    return 0;
}

```
Tulostus: "Tämä on testi"

### Esimerkki 2:
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string s = "esimerkki TEKSTI";
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    std::transform(s.begin(), s.begin()+1, s.begin(), ::toupper);
    std::cout << s << '\n';
    
    return 0;
}
```
Tulostus: "Esimerkki teksti"

## Syväsukellus:
Capitalizeerauksen alkuperä juontaa juurensa kirjoitustyylistä, jossa lauseet alkoivat isoilla kirjaimilla ja jatkuvat pienillä. Tämä oli alun perin helpottamaan lukemista, mutta nykyään sitä käytetään lähinnä visuaalisiin tarkoituksiin. Joissakin ohjelmointikielissä kuten Pythonissa, capitalizeeraus sisältyy valmiiksi merkkijonojen muokkausmetodeihin. Myös muita vaihtoehtoisia tapoja capitalizeeraukseen on kehitetty, kuten kirjahyllykapselointi, joka muuttaa merkkijonon jokaisen sanan ensimmäisen kirjaimen isoksi.

## Katso myös:
- C++ dokumentaatio merkkijono-olioille: [string](https://www.cplusplus.com/reference/string/)
- Lisää esimerkkejä capitalizeerauksesta: [Capitalization In C++](https://www.geeksforgeeks.org/capitalization-in-cpp/)