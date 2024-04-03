---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:52.607697-07:00
description: "Miten: C#:ssa ty\xF6skentelet assosiatiivisten taulukoiden kanssa k\xE4\
  ytt\xE4m\xE4ll\xE4 `Dictionary<TKey, TValue>` -luokkaa. T\xE4ss\xE4 on nopea esimerkki,\
  \ jolla p\xE4\xE4set\u2026"
lastmod: '2024-03-13T22:44:56.566219-06:00'
model: gpt-4-0125-preview
summary: "C#:ssa ty\xF6skentelet assosiatiivisten taulukoiden kanssa k\xE4ytt\xE4\
  m\xE4ll\xE4 `Dictionary<TKey, TValue>` -luokkaa."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Miten:
C#:ssa työskentelet assosiatiivisten taulukoiden kanssa käyttämällä `Dictionary<TKey, TValue>` -luokkaa. Tässä on nopea esimerkki, jolla pääset alkuun:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Luodaan sanakirja
        Dictionary<string, int> hedelmäkori = new Dictionary<string, int>();

        // Lisätään avain-arvo -pareja
        hedelmäkori.Add("Omenat", 5);
        hedelmäkori.Add("Appelsiinit", 10);

        // Arvon käyttö sen avaimen perusteella
        Console.WriteLine("Omenat: " + hedelmäkori["Omenat"]);
        
        // Arvon päivittäminen
        hedelmäkori["Omenat"] = 7;
        Console.WriteLine("Päivitetyt Omenat: " + hedelmäkori["Omenat"]);
        
        // Avain-arvo -parin poistaminen
        hedelmäkori.Remove("Appelsiinit");

        // Iterointi sanakirjan läpi
        foreach (var pari in hedelmäkori)
        {
            Console.WriteLine(pari.Key + ": " + pari.Value);
        }
    }
}
```
Esimerkkituloste:
```
Omenat: 5
Päivitetyt Omenat: 7
Omenat: 7
```

Tämä esimerkki esittelee sanakirjan luomista, elementtien lisäämistä, käyttöä, päivittämistä, poistamista ja niiden yli iterointia.

## Syväsukellus
Assosiatiivisten taulukoiden konsepti palaa niiden käyttöön käsikirjoitusskripteissä, kuten Perl ja PHP, missä ne tarjoavat joustavuutta datan kokoelmien hallinnoinnissa. C#:ssa `Dictionary<TKey, TValue>` on de facto toteutus, joka esiteltiin .NET Framework 2.0:ssa. Se tallentaa tiedot hajautustaulukkoon, varmistaen tehokkaat hakutoiminnot, lisäykset ja poistot.

On kuitenkin huomionarvoista, että vaikka sanakirjat ovat erittäin monipuolisia, ne eivät aina välttämättä ole paras valintasi. Järjestettyjen kokoelmien ylläpitämiseksi saatat tutkia `SortedDictionary<TKey, TValue>` tai `SortedList<TKey, TValue>`, jotka tarjoavat järjestettyä järjestystä hitaamman lisäys- ja poisto-operaatioiden kustannuksella. Skenaarioissa, jotka vaativat säieturvallisuutta, `ConcurrentDictionary<TKey, TValue>` lisää ylikuormaa, mutta varmistaa turvallisen käytön useista säikeistä ilman manuaalista lukitusta.

Lopulta assosiatiivisen taulukon toteutuksen valinta C#:ssa riippuu erityistarpeistasi suhteessa järjestykseen, suorituskykyyn ja säieturvallisuuteen.
