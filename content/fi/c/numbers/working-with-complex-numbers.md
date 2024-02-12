---
title:                "Työskenteleminen kompleksilukujen kanssa"
aliases:
- /fi/c/working-with-complex-numbers/
date:                  2024-02-03T18:13:52.632009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskenteleminen kompleksilukujen kanssa"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Kompleksiluvut koostuvat reaaliosasta ja imaginääriosasta, jotka esitetään muodossa `a + bi`, missä `i` on luvun `-1` neliöjuuri. Ohjelmoijat työskentelevät kompleksilukujen parissa eri aloilla, kuten sähkötekniikassa, kvanttilaskennassa ja virtausdynamiikassa, hyödyntäen niiden ainutlaatuisia ominaisuuksia simulaatioissa, signaalinkäsittelyssä ja tiettyjen matemaattisten yhtälötyyppien ratkaisemisessa.

## Kuinka:

C:ssä kompleksilukuja tuetaan Vakio Kirjaston kautta, erityisesti `<complex.h>`:n kautta. Niiden käyttämiseen julista muuttujat `double complex` tyypillä (tai `float complex` yksittäisen tarkkuuden tapauksessa). Tässä on, miten suorittaa perustoiminnot:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Julista kompleksiluku 1+2i
    double complex z2 = 1.0 - 2.0*I; // Julista toinen kompleksiluku 1-2i
    
    // Lisäys
    double complex summa = z1 + z2;
    printf("Summa: %.2f + %.2fi\n", creal(summa), cimag(summa)); // Tuloste: Summa: 2.00 + 0.00i

    // Kertolasku
    double complex tulo = z1 * z2;
    printf("Tulo: %.2f + %.2fi\n", creal(tulo), cimag(tulo)); // Tuloste: Tulo: 5.00 + 0.00i

    // Kompleksikonjugaatti
    double complex konjugaatti = conj(z1);
    printf("Z1:n konjugaatti: %.2f + %.2fi\n", creal(konjugaatti), cimag(konjugaatti)); // Tuloste: Z1:n konjugaatti: 1.00 - 2.00i
    
    // Suuruus
    double suuruus = cabs(z1);
    printf("Z1:n suuruus: %.2f\n", suuruus); // Tuloste: Z1:n suuruus: 2.24

    // Vaihe
    double vaihe = carg(z1);
    printf("Z1:n vaihe: %.2f\n", vaihe); // Tuloste radiaaneina
    
    return 0;
}
```
Huomaa, että `I` on vakio, joka edustaa imaginääriyksikköä `<complex.h>`:ssa. Funktiot kuten `creal()` ja `cimag()` poimivat vastaavasti reaali- ja imaginääriosat, kun taas `conj()` laskee kompleksikonjugaatin. Kompleksilukujen suuruuden ja vaiheen (argumentin) osalta käytetään `cabs()`- ja `carg()`-funktioita.

## Syväluotaus

Tuki kompleksiluvuille C:ssä on suhteellisen uusi, ja se standardisoitiin C99:ssä. Ennen tätä kompleksilukuaritmetiikka C:ssä oli hankalaa, usein vaatien mukautettuja tietorakenteita ja funktioita. `<complex.h>`:n ja kompleksidatatyyppien sisällyttäminen tarjosi merkittävän parannuksen kielen kykyihin tieteellisiin ja insinöörisovelluksiin. On kuitenkin huomionarvoista, että jotkut kielet, kuten Python, tarjoavat intuitiivisempaa tukea kompleksiluvuille sisäänrakennettujen datatyyppien ja rikkaamman kirjastofunktioiden joukon kautta. Siitä huolimatta C:n tarjoama suorituskyky ja hallinta tekevät siitä etusijalla olevan valinnan suorituskykyä vaativissa laskentatehtävissä, vaikka se tarkoittaakin hieman monisanaisempaa syntaksia kompleksiaritmetiikalle.
