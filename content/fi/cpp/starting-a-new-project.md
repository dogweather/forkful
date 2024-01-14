---
title:    "C++: Uuden projektin aloittaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Mikä saa meidät aloittamaan uuden ohjelmointiprojektin? Ehkä olet innostunut uudesta ideasta tai haaveilet luovasta koodaamiskokemuksesta. Riippumatta syistäsi, on aina jännittävää aloittaa jotain uutta ja luoda jotain omaa.

## Miten

Jotta voit aloittaa projektisi oikealla tavalla, on tärkeää seurata joitain perussääntöjä ja käytäntöjä. Tässä on muutamia esimerkkejä C++ koodista ja niiden tuottamasta tulosteesta:

```C++
#include <iostream>

int main() {
    std::cout << "Tervetuloa uuden projektin pariin!" << std::endl;
    int i = 5;
    std::cout << "Muuttujan i arvo on: " << i << std::endl;
    return 0;
}
```
Tulostus:
```
Tervetuloa uuden projektin pariin!
Muuttujan i arvo on: 5
```

Voit myös koodata omia toimintoja ja käyttää valmiita kirjastoja projektissasi. Esimerkiksi, jos haluat laskea kahden lukuarvon summan, voit käyttää seuraavaa koodia:

```C++
#include <iostream>

int summa(int x, int y) {
    return x + y;
}

int main() {
    int a = 5;
    int b = 7;
    int result = summa(a, b);
    std::cout << "Lukujen summa on: " << result << std::endl;
    return 0;
}
```
Tulostus:
```
Lukujen summa on: 12
```

## Syventävä tutkimus

Jotta projektisi olisi onnistunut ja helposti hallittavissa, kannattaa ottaa huomioon seuraavat asiat jo projektin aloitusvaiheessa:

- Määritä projektin tarkoitus ja tavoitteet. Mitä haluat saavuttaa projektillasi?
- Suunnittele ja organisoit projektisi rakenne etukäteen. Näin vältät turhan koodin kirjoittamisen ja helpotat projektisi laajentamista tulevaisuudessa.
- Valitse sopivat työkalut ja tekniikat projektisi toteutukseen. Esimerkiksi C++:aa voidaan käyttää niin ohjelmistokehityksessä kuin pelien luomisessa.
- Kiinnitä huomiota koodisi dokumentointiin. Se auttaa sinua ja muita ymmärtämään koodin toimintaa ja tekemään tarvittavia muutoksia tulevaisuudessa.
- Älä pelkää kysyä apua ja etsiä vastauksia kysymyksiisi verkosta tai yhteisöistä. Muut voivat tarjota arvokkaita neuvoja ja vinkkejä projektiisi liittyen.

Näiden asioiden huomioiminen auttaa sinua välttämään ongelmia ja tekee projektisi kehittämisestä sujuvampaa ja nautinnollisempaa.

## Katso myös

- [C++ resurssit aloittelijoille](https://www.codecademy.com/learn/learn-c-plus-plus)
- [C++ kirjastoja ja työkaluja](https://isocpp.org/std/the-standard)
- [C++ ohjelmointiyhteisö](https://stackoverflow.com/questions/tagged/c%2b%2b)