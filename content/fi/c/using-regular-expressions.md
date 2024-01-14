---
title:    "C: Säännöllisten lausekkeiden käyttö"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttäisit säännöllisiä lausekkeita C-ohjelmoinnissa?

Säännölliset lausekkeet ovat kätevä työkalu tekstinparsimisessa ja muokkaamisessa. Ne mahdollistavat monimutkaisten merkkijonojen hakemisen ja manipuloinnin, mikä helpottaa esimerkiksi tiedostojen lukemista ja tietokantojen käsittelyä.

## Näin käytät säännöllisiä lausekkeita C-ohjelmoinnissa

Säännöllisen lausekkeen käyttäminen C-ohjelmoinnissa vaatii muutaman perusasian ymmärtämisen. Ensinnäkin, säännöllinen lauseke koostuu erilaisista metakaraktäärimerkeistä, jotka kuvaavat erilaisia merkkijonoja. Esimerkiksi asteriski (*) tarkoittaa 0 tai useampaa esiintymää edeltävästä merkistä. Toinen olennainen asia on käyttää oikeaa funktiota, kuten "regcomp" säännöllisen lausekkeen käsittelemiseen.

Alla on esimerkki C-koodista, jossa etsitään ja tulostetaan kaikki "world" sanat annetusta merkkijonosta:

```C
#include <stdio.h>
#include <regex.h>

int main(){
    char *text = "Hello world! This is a sample text.";
    char *pattern = "world";
    regex_t regex;

    if(regcomp(&regex, pattern, 0)){
        printf("Säännöllinen lauseke ei kääntynyt oikein.");
        return 1;
    }

    regmatch_t match;
    while(regexec(&regex, text, 1, &match, 0) == 0){
        int start = match.rm_so;
        int end = match.rm_eo;
        for(int i = start; i < end; i++){
            printf("%c", text[i]);
        }
        printf(" ");
        text += end;
    }

    return 0;
}

/* Output:
world world 
*/
```

## Syvemmälle säännöllisten lausekkeiden käyttöön

Säännöllisten lausekkeiden käyttö C-ohjelmoinnissa voi olla hyvin monipuolista ja monimutkaista. Niiden avulla voi esimerkiksi: etsiä tiettyjä merkkijonoja, validoida käyttäjän antamaa syötettä, korvata sanoja toisilla sanoilla ja niin edelleen. On tärkeää ymmärtää erilaiset metakaraktäärimerkit sekä osata käyttää niitä oikein eri tilanteissa.

Jotkut C-ohjelmointikielen ympäristöt tarjoavat myös omat säännöllisen lausekkeen kirjastonsa, kuten PCRE (Perl Compatible Regular Expressions). Näiden kirjastojen avulla on mahdollista hyödyntää säännöllisiä lausekkeita vielä monipuolisemmin ja suorituskykyisemmin.

## Katso myös

- [Säännölliset lausekkeet Dokumentaatio](https://www.c-programming-simple-steps.com/regular-expressions.html)
- [Säännölliset lausekkeet opetusvideo](https://www.youtube.com/watch?v=sa-TUpSx1JA)
- [PCRE-kirjasto Dokumentaatio](https://www.pcre.org/)