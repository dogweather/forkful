---
title:                "C++: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi yhdistää merkkijonoja? 

Merkkijonot ovat tärkeitä tietorakenteita ohjelmointikielenä riippumatta. Ne koostuvat peräkkäisistä merkeistä, kuten kirjaimista, numeroista ja erikoismerkeistä, jotka muodostavat sanoja ja lauseita. Merkkijonojen yhdistäminen on tärkeä taito, jota tarvitaan usein ohjelmoinnissa, sillä se mahdollistaa monipuoliset ja dynaamiset tietorakenteet. Se on myös tehokas ja helppo tapa luoda uusia merkkijonoja tarvittaessa.

## Miten yhdistää merkkijonoja? 

Merkkijonojen yhdistäminen voidaan tehdä monella eri tavalla C++ -ohjelmointikielessä. Yksi tapa on käyttää operaattoria "+", joka yhdistää kaksi merkkijonoa toisiinsa. Toinen tapa on käyttää string-luokan append-funktiota, joka lisää uuden merkkijonon loppuun. Katso esimerkkikoodia ja tulosteita alla olevissa koodeissa.

```C++
// Käytetään "+" operaattoria yhdistämään merkkijonoja
#include <iostream>
using namespace std;

int main() {
  string s1 = "Hei";
  string s2 = "maailma";
  
  string yhdistetty = s1 + " " + s2; 
  
  cout << yhdistetty << endl; // Tulostaa "Hei maailma"
  
  return 0;
}
```

```C++
// Käytetään string-luokan append-funktiota
#include <iostream>
using namespace std;

int main() {
  string s1 = "Hei";
  string s2 = "maailma";
  
  s1.append(" ");
  s1.append(s2);
  
  cout << s1 << endl; // Tulostaa "Hei maailma"
  
  return 0;
}
```

## Syvempi sukellus yhdistämiseen 

Merkkijonojen yhdistäminen voi olla tehokasta myös silloin, kun käsitellään suuria tietomääriä. Esimerkiksi, jos haluat tulostaa kaikki listalla olevat henkilöt yhtenä merkkijonona, voit käyttää yhdistämistä sen sijaan, että tulostaisit ne yksittäisinä merkkijonoina.

Voit myös yhdistää merkkijonoja muiden tietotyyppien kanssa. Voit esimerkiksi muuntaa luvut merkkijonoiksi ja yhdistää ne toisiinsa. Merkkijonojen yhdistämistä voidaan myös tehdä dynaamisesti käyttäen silmukkaa, jossa lisätään jokainen merkkijono tietorakenteeseen ja lopuksi tulostetaan yhdistetty merkkijono.

## Katso myös 

- [http://www.cplusplus.com/reference/string/string/append/](http://www.cplusplus.com/reference/string/string/append/)
- [http://www.cplusplus.com/reference/string/operators/](http://www.cplusplus.com/reference/string/operators/)
- [https://www.tutorialspoint.com/cplusplus/cpp_strings.htm](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)