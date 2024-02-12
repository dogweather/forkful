---
title:                "Virheiden käsittely"
aliases:
- fi/c-sharp/handling-errors.md
date:                  2024-01-26T00:50:58.841439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Virheiden käsittely C#:ssa on yllättävän hallintaa—kuten kompastuminen omiin kengännauhoihisi. Ohjelmat voivat kompastua huonoihin tietoihin tai epävakaaseen yhteyteen. Käsittelemme virheitä estääksemme ohjelmistoamme kaatumasta naamalleen, mahdollistaen sen selviytymään arvokkaasti.

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
