---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:14.015019-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Stringin pituuden selvittäminen tarkoittaa merkkijonon merkkien lukumäärän laskemista. Ohjelmoijat käyttävät tätä tietoa esimerkiksi syötteiden validoinnissa, tekstin pilkkomisessa tai tietoturvan varmistamisessa.

## How to: (Kuinka tehdään:)
```C#
string tervehdys = "Moi!";
int pituus = tervehdys.Length; // Pituus on 4

Console.WriteLine(pituus); 
// Tulostaa: 4
```

## Deep Dive (Syväsukellus)
Ennen .NET Frameworkia, C-kieliset funktiot, kuten `strlen`, olivat yleisiä merkkijonojen pituuksien määrittämiseen. C#:ssa `Length`-ominaisuus tekee saman nopeammin ja turvallisemmin, koska se on osa merkkijono-objektia ja laskee pituuden suoraan sen sisäisestä muistirakenteesta.

Vaihtoehtoisesti LINQ-metodeita voidaan käyttää, mutta yleensä `Length` on tehokkain vaihtoehto. Internoinnin kautta C# pitää huolen siitä, että kaksi identtistä merkkijonoa viittaavat samaan muistiin, mikä on hyödyllistä pituustietojen suhteen.

## See Also (Katso myös)
- Microsoftin dokumentaatio: [String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- Stack Overflow -keskusteluja stringien käsittelystä: [How do you get a string to a character array in C#?](https://stackoverflow.com/questions/4734116/find-the-length-of-an-array)
- C#-oppaita LINQ:sta: [LINQ Tutorial](https://www.tutorialsteacher.com/linq)
