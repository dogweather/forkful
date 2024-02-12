---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-02-03T18:11:13.716822-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä ja miksi?

Säännölliset lausekkeet (regex) tarjoavat tavan etsiä, vastata ja manipuloida merkkijonoja käyttäen määriteltyjä kaavoja. Ohjelmoijat käyttävät niitä laajalti tehtäviin, kuten syötteiden validointiin, tekstitietojen jäsentämiseen ja kaavojen etsimiseen suurista tekstiedostoista, mikä tekee niistä tehokkaan työkalun missä tahansa kielessä, mukaan lukien C.

## Kuinka:

Käyttääksesi säännöllisiä lausekkeita C:ssä, työskentelet pääasiassa POSIX regex -kirjaston kanssa (`<regex.h>`). Tämä esimerkki esittää peruskaavan vastaavuuden:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Kaava, joka vastaa merkkijonoja, jotka alkavat 'a':lla seurattuna kirjainnumeroisista merkeistä
    char *test_string = "apple123";

    // Käännä säännöllinen lauseke
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Säännöllistä lauseketta ei voitu kääntää\n");
        exit(1);
    }

    // Suorita säännöllinen lauseke
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Vastaavuus löytyi\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Vastaavuutta ei löytynyt\n");
    } else {
        printf("Säännöllisen lausekkeen vastaavuuden haku epäonnistui\n");
        exit(1);
    }

    // Vapauta säännöllisen lausekkeen käyttämä varattu muisti
    regfree(&regex);

    return 0;
}
```

Esimerkkituloste vastaavalle merkkijonolle ("apple123"):
```
Vastaavuus löytyi
```
Ja merkkijonolle, jolle ei löydy vastaavuutta ("banana"):
```
Vastaavuutta ei löytynyt
```

## Syvä sukellus:

Säännölliset lausekkeet C:ssä, osana POSIX-standardia, tarjoavat vankan tavan suorittaa merkkijonojen vastaavuus ja manipulointi. Kuitenkin POSIX regex -kirjaston API C-kielessä pidetään kömpelömpänä kuin niiden, jotka löytyvät kielistä, jotka on suunniteltu ensiluokkaisilla merkkijonomanipulointiominaisuuksilla, kuten Python tai Perl. Kaavojen syntaksi on samanlainen eri kielissä, mutta C vaatii manuaalista muistinhallintaa ja enemmän mallikoodia valmistelemaan, suorittamaan ja siivoamaan jälkeen käytön säännöllisissä lausekkeissa.

Näistä haasteista huolimatta säännöllisten lausekkeiden käytön oppiminen C:ssä on palkitsevaa, koska se syventää ymmärrystä alemman tason ohjelmointikonsepteista. Lisäksi se avaa mahdollisuuksia C-ohjelmoinnille alueilla, kuten tekstin käsittely ja tietojen poiminta, jossa regex on korvaamaton. Monimutkaisempien kaavojen tai regex-toimintojen osalta vaihtoehdot, kuten PCRE (Perl Compatible Regular Expressions) kirjasto, saattavat tarjota ominaisuuspitoisemman ja hieman helpomman rajapinnan, vaikkakin se edellyttää ulkoisen kirjaston integroimista C-projektiisi.
