---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- /fi/c/searching-and-replacing-text.md
date:                  2024-02-03T18:09:00.512782-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin etsiminen ja korvaaminen C-kielessä tarkoittaa tiettyjen alimerkkijonojen tunnistamista suuremmasta merkkijonosta ja niiden korvaamista eri alimerkkijonoilla. Ohjelmoijat suorittavat näitä toimenpiteitä tekstitiedon käsittelyyn - tehtävissä, jotka vaihtelevat datan sanitoinnista ja muotoilusta dynaamisen sisällön generointiin.

## Kuinka:

C ei tule sisäänrakennettujen funktioiden kanssa, jotka suorittaisivat etsimisen ja korvaamisen merkkijonoissa suoraan. Voit kuitenkin saavuttaa tämän yhdistelemällä erilaisia merkkijonokäsittelytoimintoja, jotka ovat saatavilla `<string.h>` kirjastossa yhdessä jonkin omatekoisen logiikan kanssa. Alla on perusesimerkki siitä, miten etsiä alimerkkijonoa merkkijonosta ja korvata se. Yksinkertaisuuden vuoksi tässä esimerkissä oletetaan riittävä puskurikoko eikä käsitellä muistiallokaatioon liittyviä ongelmia, jotka sinun tulisi ottaa huomioon tuotantokoodissa.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // Laske pituus otteluun asti
        len_up_to_match = tmp - source;
        
        // Kopioi osa ennen ottelua
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // Kopioi uusi alimerkkijono
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // Siirry ottelun jälkeiseen osaan lähtömerkkijonossa
        tmp += len_sub;
        source = tmp;
    }
    
    // Kopioi jäljelle jäänyt osa lähtömerkkijonosta
    strcpy(insert_point, source);
    
    // Tulosta muokattu merkkijono
    printf("Modified string: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

Esimerkkituloste:
```
Muokattu merkkijono: Hello, this is a sample. This sample is simple.
```

Tämä koodi havainnollistaa yksinkertaista lähestymistapaa etsiä kaikki alimerkkijonon (`sub`) esiintymät lähtömerkkijonosta ja korvata ne toisella alimerkkijonolla (`newSub`), käyttäen `strstr` funktiota kunkin ottelun aloituspisteen löytämiseen. Se on erittäin perustason esimerkki, joka ei käsittele monimutkaisia skenaarioita, kuten päällekkäisiä alimerkkijonoja.

## Syväluotaus

Käytetty lähestymistapa "Kuinka" osiossa on perustavaa laatuaan, havainnollistaen kuinka saavuttaa tekstin etsiminen ja korvaaminen C-kielessä ilman kolmannen osapuolen kirjastoja. Historiallisesti, koska C:n painotus on alhaisen tason muistinhallinnassa ja suorituskyvyssä, sen standardikirjasto ei kapseloi korkean tason merkkijonomanipulaation toiminnallisuuksia, kuten mitä löytyy kielistä kuten Python tai JavaScript. Ohjelmoijien on manuaalisesti hallittava muistia ja yhdisteltävä erilaisia merkkijono-operaatioita tavoitellun lopputuloksen saavuttamiseksi, mikä lisää monimutkaisuutta mutta tarjoaa enemmän kontrollia ja tehokkuutta.

On tärkeää huomata, että tämä manuaalinen lähestymistapa voi olla virhealtis, erityisesti kun hallitaan muistiallokaatioita ja puskurikokoja. Virheellinen käsittely voi johtaa puskurin ylivuotoihin ja muistin korruptoitumiseen, mikä tekee koodista alttiin turvallisuusriskeille.

Monissa käytännön skenaarioissa, erityisesti niissä, jotka vaativat monimutkaista tekstinkäsittelyä, on usein harkitsemisen arvoista integroida kolmannen osapuolen kirjastoja, kuten PCRE (Perl Compatible Regular Expressions) regex-pohjaiseen etsimiseen ja korvaamiseen, mikä voi yksinkertaistaa koodia ja vähentää virheiden mahdollisuutta. Lisäksi, modernit C-standardit ja -kääntäjät tarjoavat yhä enemmän sisäänrakennettuja funktioita ja turvallisempia vaihtoehtoja merkkijonomanipulaatioon, pyrkien välttämään yleisiä sudenkuoppia, joita havaitaan vanhemmissa C-koodikannoissa. Mutta, perustavaa laatua oleva ymmärrys manuaalisesta tekstitietojen käsittelystä säilyttää arvokkaan taidon ohjelmoijan työkalupakissa, erityisesti optimoidessa suorituskyky-kriittisiä sovelluksia.
