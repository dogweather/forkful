---
title:    "C#: Få den nåværende datoen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Det å få nåværende dato er en viktig del av mange programmeringsoppgaver. Ved å ha informasjon om nåværende dato kan vi lage timer, synkronisere data og mye mer.

## Hvordan gjøre det

For å få nåværende dato i C# kan vi bruke DateTime biblioteket. Vi bruker DateTime.Now metoden for å få nåværende dato og tidspunkt. La oss se på et eksempel:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Dagens dato er: " + now.ToString("dd.MM.yyyy"));
```

I dette eksempelet definerer vi en DateTime variabel som heter "now" og bruker ToString metoden for å formatere datoen slik at den vises i riktig format. Output for dette eksempelet vil være:

```
Dagens dato er: 28.06.2021
```

Vi kan også bruke DateTime.Today metoden for å få nåværende dato uten tidspunkt. La oss se på et annet eksempel:

```C#
DateTime today = DateTime.Today;
Console.WriteLine("Dagens dato er: " + today.ToString("dd.MM.yyyy"));
```

Output for dette eksempelet vil være:

```
Dagens dato er: 28.06.2021
```

Det er også mulig å endre formatet på datoen ved å bruke ToString metoden. For eksempel, hvis vi ønsker å få nåværende måned i tre bokstaver, kan vi bruke "MMM" formatet. Dette vil se slik ut:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Dagens måned er: " + now.ToString("MMM"));
```

Output vil være:

```
Dagens måned er: Jun
```

## Dypdykk

DateTime biblioteket har mange nyttige funksjoner for å håndtere datoer og tider. For å lære mer om dette, kan du sjekke dokumentasjonen [her](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0).

## Se også

- [Working with Dates and Times in C#](https://docs.microsoft.com/en-us/dotnet/standard/datetime/working-with-dates-and-times)
- [DateTime Struct i C#](https://www.geeksforgeeks.org/datetime-struct-in-c-sharp/)
- [C# DateTime - For Dummies](https://www.dummies.com/programming/c-sharp/c-sharp-datetime-for-dummies-cheat-sheet/)