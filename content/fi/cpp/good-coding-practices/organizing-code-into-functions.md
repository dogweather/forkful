---
aliases:
- /fi/cpp/organizing-code-into-functions/
date: 2024-01-26 01:09:53.079061-07:00
description: "Koodin jakaminen funktioihin tarkoittaa sen pilkkomista pienemmiksi,\
  \ uudelleenk\xE4ytett\xE4viksi osiksi. Sen tavoitteena on toiston v\xE4ltt\xE4minen,\
  \ koodin\u2026"
lastmod: 2024-02-18 23:09:07.949928
model: gpt-4-1106-preview
summary: "Koodin jakaminen funktioihin tarkoittaa sen pilkkomista pienemmiksi, uudelleenk\xE4\
  ytett\xE4viksi osiksi. Sen tavoitteena on toiston v\xE4ltt\xE4minen, koodin\u2026"
title: "Koodin j\xE4rjest\xE4minen funktioihin"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Koodin jakaminen funktioihin tarkoittaa sen pilkkomista pienemmiksi, uudelleenkäytettäviksi osiksi. Sen tavoitteena on toiston välttäminen, koodin luettavuuden parantaminen sekä vianetsinnän ja testauksen yksinkertaistaminen. Hyvin järjestellyt funktiot voivat olla kuin laatikollinen siististi merkittyjä työkaluja, valmiina käytettäväksi ja jaettavaksi.

## Kuinka tehdään:
Otetaan esimerkki yleisestä tehtävästä: ympyrän pinta-alan laskemisesta. Sen sijaan, että kirjoittaisimme saman kaavan joka kerta, kapseloimme sen funktioon.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Ympyrän, jonka säde on " << r << ", pinta-ala on " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Esimerkkituloste:
```
Ympyrän, jonka säde on 5, pinta-ala on 78.5397
```

## Syväsukellus
Historiallisesti proseduurit ja funktiot olivat rakenteellisen ohjelmoinnin selkäranka, jonka 1960-luvulla puuhamiehet ottivat käyttöön taistellakseen varhaisempien imperatiivisten ohjelmointikielten "spagettikoodin" ongelmia vastaan. Vaihtoehdot, kuten OOP (olio-ohjelmointi), vievät asiaa eteenpäin yhdistämällä nämä funktiot tietorakenteisiin. C++:ssa on tavallisia funktioita, luokkametodeja (mukaan lukien staattiset metodit), lambdoja ja mallinefunktioita (templates functions), joista jokainen tarjoaa erilaisia etuja. Hyvin järjesteltyjen funktioiden toteuttaminen yleensä vaatii periaatteiden, kuten DRY ("Don't Repeat Yourself") ja SRP (Single Responsibility Principle), noudattamista, mikä tarkoittaa, että kukin funktio tekee vain yhden asian ja tekee sen hyvin.

## Katso myös
Lisätietoja funktioista C++:ssa:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Suunnitteluperiaatteita liittyen funktioihin:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Lue lambdoista ja edistyneestä funktiokäytöstä:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
