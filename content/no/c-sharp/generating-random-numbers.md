---
title:    "C#: Generering av tilfeldige tall"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noensinne har spilt et dataspill som krever en tilfeldig hendelse eller trenger å lage et tilfeldig sett med data, vet du hvor viktig det er å kunne generere tilfeldige tall. Tilfeldige tall er nøkkelen til å lage interessante og varierte erfaringer i programmering, og kan være avgjørende for å skape realistiske simuleringer. Derfor er det viktig å vite hvordan man kan generere disse tallene på en effektiv og nøyaktig måte.

## Hvordan Generere Tilfeldige Tall i C#

```C#
// Eksempel på å generere et tilfeldig heltall mellom 0 og 100
Random random = new Random();
int number = random.Next(0, 101); // 101 er ekskludert fra mulige verdier
Console.WriteLine(number); // resultat kan være 0, 1, 2, ..., 99, 100

// Eksempel på å generere et tilfeldig desimaltall mellom 0 og 1
double decimalNumber = random.NextDouble();
Console.WriteLine(decimalNumber); // resultat kan være 0, 0.1, 0.2, ..., 0.9, 1

// Eksempel på å generere et tilfeldig element fra en liste
List<string> fruits = new List<string>() { "eple", "banan", "jordbær", "melon" };
string fruit = fruits[random.Next(fruits.Count)];
Console.WriteLine(fruit); // resultat kan være eple, banan, jordbær eller melon
```

Kodeeksemplene viser hvordan du kan bruke Random-klassen i C# for å generere tilfeldige tall. For å generere et tilfeldig heltall, bruker vi metoden `Next()` og spesifiserer det minste og største tallet vi ønsker å få som resultat. For å generere et desimaltall, bruker vi `NextDouble()` som returnerer et tall mellom 0 og 1.

For å generere et tilfeldig element fra en liste, bruker vi også `Next()` metoden, men denne gangen bruker vi lengden på listen som argument. Dette vil gi oss et tilfeldig heltall mellom 0 og antall elementer i listen, som vi deretter kan bruke som indeks for å få et tilfeldig element fra listen.

## Dypdykk i Generering av Tilfeldige Tall

Det finnes flere måter å generere tilfeldige tall i C#, som for eksempel `Random.NextBytes()`, som genererer et tilfeldig tallrekke av byte-er. Men uavhengig av hvilken metode du bruker, er det viktig å ha en kilde som er tilstrekkelig tilfeldig for å få nøyaktige resultater. En vanlig måte å gjøre dette på er å bruke systemets klokke som kilde for tilfeldighet ved å bruke `DateTime.Now.Ticks` for å initialisere `Random`-klassen. Det finnes også andre metoder for å generere tilfeldige tall som ikke er basert på systemets tid, som for eksempel ved hjelp av en tilfeldig tallgenerator.

En annen ting å være oppmerksom på er at selv om tallene som genereres virker tilfeldige, er de egentlig pseudotilfeldige. Det betyr at de følger en bestemt algoritme, men med en veldig stor og tilsynelatende tilfeldig startverdi. Dette betyr at hvis du kjører koden flere ganger med samme seed-verdi (den initielle verdien som brukes for å begynne å generere tall), vil du få samme sekvens av "tilfeldige" tall hver gang.

## Se Også
- [Microsoft Docs: Random-klassen](https://docs.microsoft.com/nb-no/dotnet/api/system.random?view=netcore-3.1)
- [Programmering: Generering av tilfeldige tall i C#](https://www.programmering.no/2016/11/25/generering-av-tilfeldige-tall-i-csharp/)