---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- /fi/c-sharp/using-associative-arrays/
date:                  2024-01-30T19:10:52.607697-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

Assosiatiiviset taulukot, tai sanakirjat C#:ssa, antavat sinun tallentaa ja hallita avain-arvo -pareja. Ne ovat sinun valintasi, kun tarvitset nopeasti noutaa arvoja perustuen yksilölliseen tunnisteeseen, mikä tekee tiedonhallinnasta vaivatonta monimutkaisissa sovelluksissa.

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
