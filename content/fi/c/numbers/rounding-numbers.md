---
title:                "Lukujen pyöristäminen"
aliases:
- /fi/c/rounding-numbers.md
date:                  2024-02-03T18:07:44.632714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lukujen pyöristäminen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/rounding-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Numeroiden pyöristäminen on prosessi, jossa numeron numerot säädetään vähemmän tarkoiksi tietyin säännöin, joko lähimpään kokonaislukuun tai määritettyyn desimaalipaikkojen määrään. Ohjelmoijat tekevät tämän monista syistä, jotka vaihtelevat tarvittavan tallennustilan rajoittamisesta, käyttäjälle suunnatun tulosteen yksinkertaistamiseen tai tarkkojen matemaattisten toimintojen varmistamiseen, jotka ovat herkkiä hyvin pienille vaihteluille.

## Kuinka tehdään:

Numeroiden pyöristäminen C-kielessä voidaan saavuttaa käyttämällä erilaisia funktioita, mutta yleisin lähestymistapa sisältää `floor()`, `ceil()` ja `round()` funktiot. Nämä funktiot ovat osa standardia matematiikkakirjastoa, joten sinun tulee sisällyttää `math.h` ohjelmaasi.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Käyttäen floor() pyöristämään alas
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Käyttäen ceil() pyöristämään ylös
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Käyttäen round() pyöristämään lähimpään kokonaislukuun
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Pyöristäminen määriteltyyn desimaalipaikkojen määrään sisältää kertolaskun ja jakolaskun
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Pyöristetään kahteen desimaalipaikkaan: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Tulos:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Pyöristetään kahteen desimaalipaikkaan: 9.53
```

## Syväsukellus

Numeroita pyöristävällä menetelmällä on syvät historialliset juuret matematiikassa ja laskennassa, ja se on olennainen osa sekä teoreettisia että sovellettuja näkökohtia. C-kielessä, vaikka `floor()`, `ceil()`, ja `round()` tarjoavat perustoiminnallisuuden, pyöristämisen ydin siinä, että liukuluvut pyöristetään kokonaisluvuiksi tai tiettyihin desimaalipaikkoihin, on monimutkaisempaa johtuen liukulukujen binääriesityksestä. Tämä esitystapa voi johtaa odottamattomiin tuloksiin, kun käsitellään lukuja, joita ei voida tarkasti esittää binäärinä (kuten 0.1).

Nämä funktiot ovat osa C-standardikirjastoa, määritelty `<math.h>`:ssa. Pyöristettäessä numeroita, erityisesti rahoitus- tai tarkoissa insinöörilaskelmissa, on otettava huomioon binäärisiin liukulukuihin liittyvät seikat. Vaihtoehdot C:n sisäänrakennetuille funktioille erittäin tarkkaa tai desimaalikohtaista pyöristämistä varten saattavat sisältää mukautettujen pyöristysfunktioiden toteuttamisen tai tarkkaan aritmetiikkaan suunniteltujen kirjastojen, kuten GMP:n tai MPFR:n, käytön, vaikka nämä tuovatkin mukanaan lisää monimutkaisuutta ja riippuvuuksia.

Käytännössä oikean lähestymistavan valinta C:ssä pyöristämiseen edellyttää tarkkuuden, suorituskyvyn ja käytännöllisyyden tarpeen tasapainottamista, syvällä ymmärryksellä sovelluksen kehittämisen aluekohtaisista vaatimuksista.
