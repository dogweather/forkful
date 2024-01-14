---
title:                "C: Säännöllisten lausekkeiden käyttö"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi 

Regular expressionit ovat voimakas työkalu C-ohjelmoinnissa. Ne mahdollistavat tiettyjen merkkijonojen etsimisen ja manipuloinnin, jotka ovat hyödyllisiä monissa ohjelmointitilanteissa.

## Miten

Käyttö: Regular expressionin käyttö on helppoa, sillä se koostuu vain muutamasta avainsanasta ja erikoismerkistä. Käytämme kirjastoa <regex.h>, joka sisältää tarvittavat toiminnallisuudet.

Koodiesimerkki:
```C
#include <stdio.h>
#include <regex.h>

int main() {
  // Luodaan regex-objekti ja määritellään muuttuja, johon tallennetaan mahdollinen virheilmoitus.
  regex_t re;
  int error;

  // Syötetään testimerkkijono.
  char* test_string = "Tämä on testimerkkijono 123.";
  
  // Määritellään haluttu hakuilmaus.
  char* pattern = "(.*123)";

  // Käännetään ilmaus ja tallennetaan virheilmoitus.
  error = regcomp(&re, pattern, 0);

  // Tarkistetaan, onko ilmaus löytynyt testimerkkijonosta.
  if (regexec(&re, test_string, 0, NULL, 0) == 0) {
    printf("Merkkijonosta löytyi ilmaus!");
  } else {
    printf("Merkkijonosta ei löytynyt ilmausta.");
  }

  // Lopuksi vapautetaan regex-objekti.
  regfree(&re);
  return 0;
}
```

Tuloste:
```
Merkkijonosta löytyi ilmaus!
```

## Syväsyvennys

Regular expressionit voivat olla hyödyllisiä esimerkiksi datan validointiin, ohjelman suoritusnopeuden optimointiin ja erilaisten merkkijonojen ja tiedostojen käsittelyyn. Regex-kieli on myös laajalti käytössä muissa ohjelmointikielissä, joten sen oppiminen auttaa myös siirtymistä muihin kieliin.

## Katso myös

- [Regular expressions in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Regex cheatsheet](https://www.rexegg.com/regex-quickstart.html)
- [Interactive regex tutorial](https://regex101.com/)