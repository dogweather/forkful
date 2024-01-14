---
title:    "C: Säännöllisten lausekkeiden käyttö"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita C-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, jota voidaan käyttää C-ohjelmoinnissa tekstin käsittelyyn. Ne tarjoavat tehokkaan ja joustavan tavan löytää, muokata ja korvata tiettyjä merkkijonoja ohjelmassa. Tämä tekee niistä erittäin hyödyllisiä työkaluja esimerkiksi merkkijonojen validointiin ja muokkaamiseen.

## Kuinka käyttää säännöllisiä lausekkeita C-ohjelmoinnissa?

Säännöllisten lausekkeiden käyttäminen C-ohjelmoinnissa sisältää muutaman perusaskeleen:

1. Määritä säännöllinen lauseke C-merkkijonona. Voit käyttää merkkijonokonstruktoria `""` tai escape-merkkejä `'`.
2. Käytä `regex.h` kirjastoa luodaksesi säännölliseen lausekkeeseen sopiva olio.
3. Käytä `regex.h` kirjastoa sopivan funktion, kuten `regexec`, avulla sopivien merkkijonojen löytämiseen ja muokkaamiseen.

Alla on esimerkki koodista, joka käyttää säännöllisiä lausekkeita sähköpostiosoitteen validointiin:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    char regex_string[] = "^([a-zA-Z0-9_.-]+)@([a-zA-Z0-9_.-]+)\\.([a-zA-Z]{2,5})$";
    char email[] = "example@email.com";
    
    regex_t regex;
    int match_result;
    
    match_result = regcomp(&regex, regex_string, REG_EXTENDED);
    
    if (match_result == 0) {
        match_result = regexec(&regex, email, 0, NULL, 0);
        if (match_result == 0) {
            printf("Sähköpostiosoite on validi!");
        } else {
            printf("Virheellinen sähköpostiosoite!");
        }
    }
    return 0;
}
```
Yllä olevassa koodissa käytetään `regcomp` ja `regexec` funktioita luomaan ja tarkastamaan, vastaako sähköpostiosoite määritettyyn säännölliseen lausekkeeseen. Jos vastaus on positiivinen, tulostetaan viesti "Sähköpostiosoite on validi!". Muuten tulostetaan "Virheellinen sähköpostiosoite!".

## Syvällisempi sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet ovat oikeastaan pieni ohjelmointikieli, joka koostuu erilaisista merkeistä ja säännöistä, joilla voidaan muokata ja löytää merkkijonoja. Niiden käyttö vaatii hieman opettelua, mutta niiden tehokkuus ja monipuolisuus tekevät niistä ehdottomasti vaivan arvoisia.

On tärkeää muistaa, että säännölliset lausekkeet voivat vaihdella hieman eri ohjelmointikielillä. Tämän vuoksi on tärkeää tutustua tarkasti C:n säännöllisten lausekkeiden dokumentaatioon ennen niiden käyttöä.

## Katso myös

- [C:n säännöllisten lausekkeiden dokumentaatio](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Teoriaa säännöllisistä lausekkeista](https://www.regular-expressions.info/)
- [Regex101 - Verkkosovellus säännöllisten lausekkeiden kokeilemiseen ja testaamiseen](https://regex101.com/)