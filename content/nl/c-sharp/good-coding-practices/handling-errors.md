---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:57.462583-07:00
description: "Het afhandelen van fouten in C# gaat over het beheren van het onverwachte\u2014\
  zoals struikelen over je veters. Programma's kunnen struikelen over slechte\u2026"
lastmod: '2024-02-25T18:49:48.157023-07:00'
model: gpt-4-0125-preview
summary: "Het afhandelen van fouten in C# gaat over het beheren van het onverwachte\u2014\
  zoals struikelen over je veters. Programma's kunnen struikelen over slechte\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het afhandelen van fouten in C# gaat over het beheren van het onverwachte—zoals struikelen over je veters. Programma's kunnen struikelen over slechte gegevens of wankelende verbindingen. We behandelen fouten om te voorkomen dat onze software op zijn gezicht valt, waardoor het sierlijk kan herstellen.

## Hoe te:

Laten we beginnen met een try-catch blok. Het is alsof je een veiligheidsnet onder een koorddanser plaatst. Als ze uitglijden, vallen ze niet te pletter—ze worden opgevangen.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // Oeps, index ligt buiten de grenzen!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Een fout gevangen: " + e.Message);
        }
    }
}
```

Voorbeelduitvoer als dingen misgaan:
```
Een fout gevangen: Index was buiten de grenzen van de array.
```

Nu voegen we een finally blok toe—dat gebeurt wat er ook gebeurt, zoals belasting betalen.

```C#
try {
    // Mogelijk problematische code hier
} catch (SomeSpecificException e) {
    // Handel die specifieke fout hier af
} finally {
    // Deze code wordt uitgevoerd wat er ook boven gebeurt
    Console.WriteLine("Dit wordt altijd uitgevoerd.");
}
```

## Diepere Duik

Foutafhandeling is sinds de geboorte van C# aanwezig. In de loop der tijd is het geëvolueerd. Vroeger vertrouwden programmeurs op retourcodes of globale vlaggen om problemen aan te geven—lomp en foutgevoelig.

C# gebruikt uitzonderingen, een modernere benadering. Een uitzondering wordt geworpen wanneer het onverwachte gebeurt, net zoals een vlag gooien tijdens een voetbalwedstrijd. Gestroomlijnde uitzonderingsafhandeling met try, catch en finally blokken maakt het beheer van deze momenten duidelijker en schoner dan de oude manier van fouten controleren.

Alternatieven? Zeker. Er is de `UnhandledExceptionEventHandler` voor uitzonderingen die erdoorheen slippen. Of in asynchrone code, waar foutafhandeling een beetje op zijn kop staat met `Task` objecten die hun eigen bagage van uitzonderingen meedragen.

Implementatiedetails—vergelijkbaar met de kleine lettertjes—doen ertoe. Uitzonderingen kunnen kostbaar zijn, de prestaties omlaag halen als ze willekeurig worden gegooid. Dus, we gebruiken ze voor uitzonderlijke gevallen, niet voor alledaagse logicacontrole.

## Zie Ook

- [Officiële documentatie over uitzonderingen in C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Beste praktijken in de afhandeling van C# uitzonderingen](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
