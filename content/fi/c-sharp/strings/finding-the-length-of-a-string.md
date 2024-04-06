---
date: 2024-01-20 17:47:14.015019-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) ."
lastmod: '2024-04-05T21:53:58.131699-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

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
