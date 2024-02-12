---
title:                "Virheiden käsittely"
date:                  2024-02-03T17:58:06.289960-07:00
model:                 gpt-4-0125-preview
simple_title:         "Virheiden käsittely"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden käsittely C:ssä tarkoittaa ohjelman suorituksen aikana esiintyvien poikkeavien tilanteiden havaitsemista ja niihin vastaamista. Ohjelmoijat tekevät tämän estääkseen bugeja, kaatumisia ja odottamatonta käytöstä, varmistaen että ohjelmisto toimii luotettavasti ja tehokkaasti erilaisissa skenaarioissa.

## Miten:

C ei sisällä sisäänrakennettua tukea poikkeuksille kuten jotkut muut kielet. Sen sijaan se nojaa muutamiin perinteisiin virheidenkäsittelystrategioihin, kuten erityisten arvojen palauttamiseen funktioista ja globaalien muuttujien, kuten `errno`, asettamiseen.

**Erityisten Arvojen Palauttaminen**

Funktiot voivat ilmoittaa virheistä palauttamalla tietyn arvon, joka on epätodennäköisesti kelvollinen tulos. Tässä on esimerkki kokonaisluvuilla:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Virhetilanne
    } else {
        *result = 1.0 / number;
        return 0; // Onnistui
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Virhe: Jakaminen nollalla.\n");
    } else {
        printf("Käänteisarvo on: %f\n", result);
    }
    
    return 0;
}
```

**Tuloste:**
```
Virhe: Jakaminen nollalla.
```

**`errno` Tarkistaminen**

Kirjastofunktioissa, erityisesti niissä jotka vuorovaikuttavat järjestelmän tai käyttöjärjestelmän kanssa (kuten tiedostojen I/O), asetetaan `errno`, kun virhe tapahtuu. Käyttääksesi sitä, sisällytä `errno.h` ja tarkista `errno` epäillyn epäonnistumisen jälkeen:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Virhe tiedoston avaamisessa: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Tuloste:**
```
Virhe tiedoston avaamisessa: Tiedostoa tai hakemistoa ei ole
```

## Syväluotaus

Historiallisesti C-ohjelmointikielen minimalistinen suunnittelu on jättänyt pois sisäänrakennetun poikkeusten käsittelymekanismin, mikä heijastaa sen matalan tason, järjestelmäohjelmoinnin alkuperää, jossa maksimaalinen suorituskyky ja suora hallinta ovat kriittisiä. Sen sijaan C omaksuu manuaalisemman virheidenkäsittelylähestymistavan, joka sopii sen filosofiaan antaa ohjelmoijille mahdollisimman paljon kontrollia, jopa mukavuuden kustannuksella.

Vaikka tämä lähestymistapa on linjassa C:n suunnittelutavoitteiden kanssa, se voi johtaa myös sanalliseen virhetarkistuskoodiin ja mahdollisiin tarkistamatta jääviin virheisiin, jotka modernit kielet käsittelevät rakenteellisen poikkeustenkäsittelyn mekanismeilla. Esimerkiksi poikkeukset kielissä kuten Java tai C# mahdollistavat keskitetyn virheenkäsittelyn, mikä tekee koodista siistimpää ja virheenhallinnasta suoraviivaisempaa. Kuitenkin poikkeukset tuovat mukanaan oman ylikuormansa ja monimutkaisuutensa, joka ei välttämättä ole ihanteellista järjestelmätason ohjelmoinnissa, jossa C loistaa.

Huolimatta sen karkeudesta, tämä manuaalinen virheidenkäsittely C:ssä on vaikuttanut monien muiden kielten virheenhallinnan suunnitteluun, tarjoten mallin, jossa virhetilojen selkeys voi johtaa ennustettavampaan ja helpommin vianetsittävään koodiin. Kriittisissä järjestelmissä, joissa vikatilanteet on hallittava sulavasti, C:n virheenkäsittelyparadigma – yhdistettynä modernien parhaiden käytäntöjen, kuten virheidenkäsittelykirjastojen ja -konventioiden, kanssa – varmistaa järjestelmän luotettavuuden ja robustiuden.
