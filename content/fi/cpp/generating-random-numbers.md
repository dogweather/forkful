---
title:                "C++: Satunnaisten lukujen generointi."
simple_title:         "Satunnaisten lukujen generointi."
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Satunnaislukujen tuottaminen ohjelmoinnissa

Ohjelmoinnissa saattaa usein olla tarve luoda satunnaisia lukuja tietylle välille tai tiettyä prosessia varten. Satunnaislukujen tuottamiseen on useita erilaisia menetelmiä, ja tässä blogikirjoituksessa tarkastellaan miten se tehdään C++-kielellä.

## Kuinka: Koodiesimerkkejä ja tulosteita

Satunnaislukujen tuottaminen onnistuu C++:lla käyttämällä standardikirjaston <random> ja <iostream> -kirjastoja. Tässä esimerkissä luodaan neljä satunnaista kokonaislukua väliltä 1-10 ja tulostetaan ne näytölle.

```C++
#include <iostream>
#include <random>

int main()
{
  // Alustetaan satunnaisgeneraattori ja määritellään välit
  std::random_device rd;  // käytetään laitteen luomaa satunnaislukua alustukseen
  std::mt19937 gen(rd()); // käytetään Marsenne Twister -generaattoria
  std::uniform_int_distribution<> dist(1, 10); // välit 1-10
  
  // Luodaan ja tulostetaan satunnaisluvut
  std::cout << "Satunnaisluvut väliltä 1-10:" << std::endl;
  for (int i = 0; i < 4; i++)
  {
    std::cout << dist(gen) << std::endl;
  }
}
```

Tuloste:
```
Satunnaisluvut väliltä 1-10:
6
3
8
10
```

## Syventävä tieto satunnaisluvun tuottamisesta

Satunnaislukujen tuottamiseen käytetyt generaattorit perustuvat matemaattisiin algoritmeihin, jotka tuottavat lukuja tiettyjen sääntöjen mukaisesti. Näitä algoritmeja pyritään valitsemaan siten, että ne tuottavat mahdollisimman satunnaisia lukuja.

C++:ssa käytetään yleisesti Marsenne Twister -generaattoria (std::mt19937), sillä se on yksi tehokkaimmista ja tarkimmista generaattoreista. Sen sijaan juuri satunnaislukujen määrittelyyn käytetään uniform_int_distribution -luokkaa, joka varmistaa tasaisen jakauman halutulla välillä.

On myös tärkeää huomata, että C++:n satunnaislukujen tuotto ei ole täysin satunnainen vaan pseudosatunnainen. Tämä tarkoittaa, että vaikka luvut tuntuisivatkin satunnaisten väliltä, ne tuotetaan kuitenkin tietyn algoritmin mukaisesti ja lopputulos on ennustettavissa.

## Katso myös

- <a href="https://www.cplusplus.com/reference/random/">C++ reference - Satunnaislukukirjasto</a>
- <a href="https://www.geeksforgeeks.org/generating-random-number-range-c/">Generating random number in a given range</a>
- <a href="https://www.youtube.com/watch?v=3IfDt1n62Ps">Video: Generating Random Numbers in C++ (CppCon 2017)</a>