---
title:    "C#: Uuden projektin aloittaminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Miksi aloittaa uusi projektisi 

Aloittaessa uuden projektin, voi olla monia syitä miksi teet niin. Ehkäpä haluat oppia uusia tekniikoita, kehittää taitojasi tai haluat saavuttaa jotain tärkeää. Jokaiselle henkilölle syynä voi olla erilainen, mutta tärkeintä on, että projekti innostaa ja motivoi sinua!

## Kuinka aloittaa uusi projekti

Jos olet aloittelija, saattaa tuntua pelottavalta aloittaa uusi projekti. Mutta ei hätää, olemme täällä auttamassa! Aloitetaan luomalla yksinkertainen C# ohjelma, jossa kysytään käyttäjän nimi ja tulostetaan se konsoliin.

```C#
using System;

namespace AloitaProjekti
{
    class Program
    {
        static void Main(string[] args)
        {
            // Kysytään käyttäjän nimi
            Console.WriteLine("Hei! Mikä on nimesi?");
            // Tallennetaan käyttäjän vastaus muuttujaan
            string nimi = Console.ReadLine();
            // Tulostetaan nimi konsoliin
            Console.WriteLine("Hei " + nimi + ", hauska tutustua!");
        }
    }
}
```

#### Tuloste:

> Hei! Mikä on nimesi?  
> *käyttäjän syöttämä nimi*  
> Hei *käyttäjän nimi*, hauska tutustua!

Nyt olet luonut ensimmäisen C# ohjelmasi! Jatka kokeilemalla erilaisia ohjelmointikäsitteitä ja harjoittele listoja, ehtolauseita ja silmukoita.

## Syvällisempi perehtyminen uuden projektin aloittamiseen

Kun suunnittelet uutta projektia, on ensiarvoisen tärkeää hahmotella selkeä tavoite. Mitä haluat saavuttaa projektillasi? Onko tarkoituksena luoda sovellus, peli tai verkkosivusto? Älä myöskään unohda miettiä mitä kieliä ja työkaluja tarvitset projektiisi. Päätöksen jälkeen voit aloittaa suunnittelun ja määritellä projektisi tarpeet ja vaatimukset.

Muista myös dokumentoida projektiasi ja pitää se järjestelmällisenä. Tee tarvittaessa suunnitelman muutoksia matkan varrella ja älä unohda testausta! Loppujen lopuksi projektin tulisi olla hauska ja oppimiskokemus.

## Katso myös

- [Microsoftin C# opetusohjelmat](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/)
- [C# koodausstandardeja](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/inside-a-program/coding-conventions)