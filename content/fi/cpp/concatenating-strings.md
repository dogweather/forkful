---
title:                "Merkkijonojen yhdistäminen"
html_title:           "C++: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Miksi haluaisimme yhdistää merkkijonoja koodissamme? Merkkijonojen yhdistäminen on yksinkertaisesti tapa yhdistää kaksi tai useampi merkkijono yhdeksi. Tämä voi olla hyödyllistä esimerkiksi, kun haluamme luoda pidempiä lauseita tai tulostaa useita muuttujia yhdessä.

# Miten:

## Esimerkki 1:

```
#include <iostream>

using namespace std;

int main() {
    string name = "John";
    string country = "Finland";

    string sentence = "Hei, olen " + name + " ja olen kotoisin maasta " + country + ".";

    cout << sentence;

    return 0;
}

```

Tulostuu:
```
Hei, olen John ja olen kotoisin maasta Finland.
```

## Esimerkki 2:

```
#include <iostream>

using namespace std;

int main() {
    string word = "koodaaja";

    string phrase = word + " on paras ammatti.";

    cout << phrase;

    return 0;
}
```

Tulostuu:
```
Koodaaja on paras ammatti.
```

# Syventävä tieto:

## Historiallinen tausta:
Merkkijonojen yhdistäminen on ollut osa ohjelmoinnissa jo pitkään. Alun perin, merkkijonojen yhdistäminen oli tehtävä manuaalisesti käyttämällä kirjaimia ja merkkejä yhdessä. Nykyään, voi yksinkertaisesti käyttää operaattoria "+" yhdistääkseen merkkijonoja toisiinsa.

## Vaihtoehtoiset tavat:
On myös muita tapoja yhdistää merkkijonoja, kuten käyttäen C++ standardikirjaston funktioita, kuten std::string::append() tai std::strcat(). Nämä vaihtoehdot voivat olla hyödyllisiä erilaisissa tilanteissa ja merkkijonojen yhdistäminen operaattorilla "+" onkin vain yksi tapa tehdä se.

## Toteutuksen yksityiskohdat:
Merkkijonojen yhdistäminen operaattorilla "+" allokoi uuden merkkijonon ja kopioidaan siihen molempien merkkijonojen merkit. Tämä voi olla resurssien intensiveistä, joten on suositeltavaa käyttää muita vaihtoehtoja, jos on tarvetta yhdistää isompia merkkijonoja.

# Katso myös:
- [C++ std::string::append reference](https://www.cplusplus.com/reference/string/string/append/)
- [C++ std::strcat reference](https://www.cplusplus.com/reference/cstring/strcat/)