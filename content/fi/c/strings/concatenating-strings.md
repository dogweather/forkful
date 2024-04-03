---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:23.901510-07:00
description: "Miten: C-kieless\xE4 merkkijonot ovat merkkej\xE4 sis\xE4lt\xE4vi\xE4\
  \ taulukoita, jotka p\xE4\xE4ttyv\xE4t nolla-merkkiin (`\\0`). Toisin kuin korkeamman\
  \ tason kieliss\xE4, C ei\u2026"
lastmod: '2024-03-13T22:44:57.030684-06:00'
model: gpt-4-0125-preview
summary: "C-kieless\xE4 merkkijonot ovat merkkej\xE4 sis\xE4lt\xE4vi\xE4 taulukoita,\
  \ jotka p\xE4\xE4ttyv\xE4t nolla-merkkiin (`\\0`)."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## Miten:
C-kielessä merkkijonot ovat merkkejä sisältäviä taulukoita, jotka päättyvät nolla-merkkiin (`\0`). Toisin kuin korkeamman tason kielissä, C ei tarjoa sisäänrakennettua merkkijonojen yhdistämistoimintoa. Sen sijaan käytät `<string.h>` kirjaston `strcat()`- tai `strncat()`-funktioita.

Tässä on yksinkertainen esimerkki `strcat()` käytöstä:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hei, ";
    char source[] = "Maailma!";

    strcat(destination, source);

    printf("%s\n", destination);  // Tuloste: Hei, Maailma!
    return 0;
}
```

`strcat()`-funktio ottaa kaksi argumenttia: kohdemerkkijonon (jossa on oltava tarpeeksi tilaa yhdistetyn tuloksen sisältämiseen) ja lähdemerkkijonon. Sen jälkeen se liittää lähdemerkkijonon kohdemerkkijonoon.

Lisää hallintaa yhdistettävien merkkien määrään tarjoaa `strncat()`, jonka käyttö on turvallisempaa:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hei, ";
    char source[] = "Maailma!";
    int num = 3; // Liitettävien merkkien määrä

    strncat(destination, source, num);

    printf("%s\n", destination);  // Tuloste: Hei, Maa
    return 0;
}
```

Tämä rajoittaa yhdistämisen lähdemerkkijonon ensimmäisiin `num` merkkeihin, auttaen estämään puskuriylivuotoja.

## Syväsukellus
Funktiot `strcat()` ja `strncat()` ovat olleet osa C:n standardikirjastoa sen alusta lähtien, heijastaen kielen matalan tason luonnetta, joka vaatii manuaalista merkkijonojen ja muistin hallintaa. Toisin kuin monet nykyaikaiset ohjelmointikielet, jotka käsittelevät merkkijonoja ensiluokkaisina objekteina sisäänrakennettujen yhdistämisoperaattoreiden (kuten `+` tai `.concat()`) kanssa, C:n lähestymistapa vaatii syvempää ymmärrystä osoittimista, muistin varauksesta ja potentiaalisista riskeistä, kuten puskuriylivuodoista.

Vaikka `strcat()`- ja `strncat()`-funktiot ovat laajalti käytössä, niitä usein arvostellaan niiden mahdollisuudesta luoda tietoturvariskejä, jos niitä ei käytetä varoen. Puskuriylivuodot, joissa tiedot ylittävät varatun muistin, voivat johtaa kaatumisiin tai niitä voidaan käyttää mielivaltaisen koodin suorittamiseen. Tämän seurauksena ohjelmoijat kääntyvät yhä enemmän turvallisempien vaihtoehtojen, kuten `snprintf()`, puoleen, mikä tarjoaa ennustettavamman käyttäytymisen rajoittamalla kohdemerkkijonoon kirjoitettavien merkkien määrää sen koon perusteella:

```c
char destination[50] = "Hei, ";
char source[] = "Maailma!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

Tämä menetelmä on verbosimpi mutta huomattavasti turvallisempi, korostaen siirtymistä C-ohjelmointikäytännöissä kohti turvallisuuden ja robustisuuden priorisointia lyhyyden sijasta.

Huolimatta näistä haasteista, merkkijonojen yhdistäminen C:ssä on perustaito, joka on oleellinen tehokkaaseen ohjelmointiin kielessä. Sen vivahteiden ja niihin liittyvien riskien ymmärtäminen on avain C-ohjelmoinnin hallintaan.
