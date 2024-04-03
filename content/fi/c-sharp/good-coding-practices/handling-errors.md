---
date: 2024-01-26 00:50:58.841439-07:00
description: "Kuinka: Aloitetaan try-catch-lohkolla. Se on kuin turvaverkon asettaminen\
  \ nuorallatanssijan alle. Jos he lipe\xE4v\xE4t, he eiv\xE4t putoa\u2014heid\xE4\
  t pyydystet\xE4\xE4n."
lastmod: '2024-03-13T22:44:56.580721-06:00'
model: gpt-4-1106-preview
summary: Aloitetaan try-catch-lohkolla.
title: "Virheiden k\xE4sittely"
weight: 16
---

## Kuinka:
Aloitetaan try-catch-lohkolla. Se on kuin turvaverkon asettaminen nuorallatanssijan alle. Jos he lipeävät, he eivät putoa—heidät pyydystetään.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numerot = {1, 2, 3};
            Console.WriteLine(numerot[5]);  // Hups, indeksi on taulukon ulkopuolella!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Napattiin virhe: " + e.Message);
        }
    }
}
```

Esimerkkituloste, kun asiat menevät pieleen:
```
Napattiin virhe: Indeksi oli taulukon rajojen ulkopuolella.
```

Nyt lisäämme finally-lohkon—se on mitä tapahtuu joka tapauksessa, kuten verojen maksaminen.

```C#
try {
    // Mahdollisesti ongelmallinen koodi tässä
} catch (SomeSpecificException e) {
    // Käsitellään tietty virhe tässä
} finally {
    // Tämä koodi suoritetaan riippumatta siitä, mitä yllä tapahtuu
    Console.WriteLine("Tämä suoritetaan aina.");
}
```

## Syväsukellus
Virheiden käsittely on ollut osa C#:ia sen syntymästä lähtien. Ajan myötä se on kehittynyt. Aikoinaan ohjelmoijat turvautuivat palautuskoodeihin tai yleisiin merkkilippuihin viestimään ongelmista—kömpelöä ja altista virheille.

C# käyttää poikkeuksia, mikä on modernimpi lähestymistapa. Poikkeus heitetään ilmoille, kun odottamaton tapahtuu, aivan kuin heittäisit lipun peliin jalkapallossa. Rakenenteinen poikkeuksien käsittely try-, catch- ja finally-lohkojen avulla tekee näiden hetkien hallinnasta selkeämpää ja siistimpää kuin vanhan koulun virhetarkistukset.

Vaihtoehtoja? Tietysti. On olemassa `UnhandledExceptionEventHandler`, jolle osoitetaan poikkeukset, jotka livahtavat läpi. Tai asynkronisessa koodissa virheiden käsittely kääntyy hieman päälaelleen `Task`-objektien kanssa, jotka kantavat omaa poikkeusten taakkaansa.

Toteutuksen yksityiskohdat—verrattavissa pienellä printatulle tekstille—ovat tärkeitä. Poikkeukset voivat olla kalliita, hidastaen suorituskykyä, jos niitä heitetään holtittomasti. Joten käytämme niitä poikkeuksellisissa tilanteissa, ei jokapäiväisessä logiikan hallinnassa.

## Katso myös
- [Virallinen dokumentaatio C#-poikkeuksista](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Parhaat käytännöt C#-poikkeusten käsittelyssä](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
