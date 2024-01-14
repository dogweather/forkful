---
title:    "C: Tiedoston lukeminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan blogia C-ohjelmoijille! Tässä artikkelissa sukellamme syvemmälle tiedostojen lukemiseen C-kielellä ja käymme läpi koodin esimerkkien avulla, miksi tämä on tärkeää ja miten se tehdään. Jos olet kiinnostunut oppimaan lisää tiedostojen käsittelystä C-kielellä, jatka lukemista!

## Miten

Tiedostojen lukeminen C-kielellä on tärkeä taito ohjelmoijille. Se antaa meille mahdollisuuden lukea tietoja ulkoisista lähteistä ja käsitellä niitä ohjelmassamme. Voimme lukea esimerkiksi tekstitiedostoja, jotka sisältävät käyttäjän syöttämiä tietoja, tai lukea tiedostoja, jotka sisältävät valmiiksi tallennettuja tietoja.

Aloitetaan yksinkertaisella esimerkillä, jossa luemme ja tulostamme tiedoston sisällön näytölle. Käytämme fopen-funktiota avataksesi tiedoston ja tarkistetamme sen palauttaman osoittimen arvon. Jos osoitin on NULL, tiedoston avaaminen epäonnistui. Muuten luemme tiedoston sisällön ja tulostamme sen näytölle käyttäen fgets-funktiota.

```
#include <stdio.h>

int main(void) {

    FILE *tiedosto;
    char rivi[1000];

    tiedosto = fopen("esimerkki.txt", "r");

    if (tiedosto == NULL) {
        printf("Tiedoston avaaminen epäonnistui!");
        return 1;
    }

    while (fgets(rivi, 1000, tiedosto) != NULL) {
        printf("%s", rivi);
    }

    fclose(tiedosto);

    return 0;
}
```

Kun ajamme tämän koodin, tulostuu esimerkkitiedoston sisältö näytölle.

```
Tämä on esimerkki.
Tässä on toinen rivi.
```

## Syvällinen sukellus

Tiedostojen lukeminen C-kielellä voi vaikuttaa monimutkaiselta, mutta kun ymmärrät sen perusteet, voit käyttää sitä monipuolisesti erilaisissa ohjelmissa. On tärkeää muistaa sulkea tiedosto käytön jälkeen tehokkaan muistinhallinnan takaamiseksi. Voit myös käyttää fseek-funktiota siirtääksesi lukupistettä tiedostossa, jotta voit lukea ja käsitellä tietoja haluamallasi tavalla.

On myös hyvä huomata, että tiedostojen lukeminen ja kirjoittaminen eroavat toisistaan. Jos haluat kirjoittaa tiedostoon, sinun tulee käyttää fopen-funktiota parametrilla "w" ja fclose-funktiota tiedoston sulkemiseen.

Toivottavasti tämä antoi sinulle hyvän alun tiedostojen lukemisen oppimiseen C-kielellä. Jatka tutustumista ja löydä uusia tapoja käyttää tätä taitoa!

## Katso myös

- [C ohjelmointikieli] (https://www.tutorialspoint.com/cprogramming/c_fundamentals.htm)
- [fgets-funktio] (https://www.tutorialspoint.com/c_standard_library/c_function_fgets.html)
- [fopen-funktio] (https://www.tutorialspoint.com/c_standard_library/c_function_fopen.html)