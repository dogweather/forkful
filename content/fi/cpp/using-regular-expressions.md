---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Säännölliset lausekkeet (regular expressions) ovat erityinen merkkijonojen käsittelyn tapa, jolla voit etsiä, sovittaa yhteen, vaihtaa ja jakaa merkkijonoja. Ohjelmoijat käyttävät niitä koodin tehostamiseksi ja jotta heidän ei tarvitsisi kirjoittaa manuaalisesti monimutkaisia merkkijonojen käsittelyfunktioita.

## Näin se tehdään:
```C++
#include <regex>
#include <iostream>

int main(){
    std::string s = "Programming article!";
    std::regex e ("\\b(sub)?\\w*");  

    // using regex_replace()
    std::cout << std::regex_replace(s,e,"C++");
    return 0;
}
```
Tämä koodi korvaa jokaisen sanan (\\b(sub)?\\w*) merkkijonossa "Programming article!" sanalla "C++". 

## Syvä Sukellus
Säännöllisillä lausekkeilla on pitkä historia, ne olivat olemassa jo ennen kuin C++ otti ne käyttöön. Niitä käytetään usein kun tarvitaan tehokasta tekstinkäsittelyä. Vaihtoehtoisia menetelmiä ovat mm. manuaalinen merkkijonojen käsittely ja kuviohaku. C++:ssa säännölliset lausekkeet on toteutettu <regex> kirjastossa. 

## Katso Myös
Jos haluat oppia lisää säännöllisistä lausekkeista, suosittelemme seuraavia linkkejä:

* [C++ Regular expressions](http://www.cplusplus.com/reference/regex/)
* [Regular Expression Library](http://regexlib.com/)
* [Wikipedia: Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression)