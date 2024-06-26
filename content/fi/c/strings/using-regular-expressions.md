---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:13.716822-07:00
description: "Kuinka: K\xE4ytt\xE4\xE4ksesi s\xE4\xE4nn\xF6llisi\xE4 lausekkeita C:ss\xE4\
  , ty\xF6skentelet p\xE4\xE4asiassa POSIX regex -kirjaston kanssa (`<regex.h>`).\
  \ T\xE4m\xE4 esimerkki esitt\xE4\xE4\u2026"
lastmod: '2024-03-13T22:44:57.028413-06:00'
model: gpt-4-0125-preview
summary: "K\xE4ytt\xE4\xE4ksesi s\xE4\xE4nn\xF6llisi\xE4 lausekkeita C:ss\xE4, ty\xF6\
  skentelet p\xE4\xE4asiassa POSIX regex -kirjaston kanssa (`<regex.h>`)."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

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
