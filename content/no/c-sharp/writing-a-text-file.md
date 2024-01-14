---
title:    "C#: Skriving av en tekstfil"
keywords: ["C#"]
---

{{< edit_this_page >}}

##Hvorfor

Å skrive en tekstfil er en grunnleggende aktivitet i C# programmering. Det lar deg enkelt lagre data og informasjon på en tekstbasert måte som kan leses og behandles av datamaskiner. Dette kan være nyttig for å lagre brukerinndata, loggfiler, eller andre typer informasjon som programmet ditt trenger å lagre og lese på et senere tidspunkt.

##Hvordan

For å skrive en tekstfil i C#, må vi først opprette en instans av StreamWriter-klassen. Dette vil gi oss tilgang til metoder for å skrive tekst til filen. Vi må også spesifisere filbanen og navnet på filen som vi vil skrive til.

Det er viktig å lukke StreamWriter-instansen når vi er ferdige med å skrive til filen. Dette gjøres ved å kalle Close() metoden, som vil sørge for at alle dataene er lagret og at filen er klar til å bli åpnet og lest senere.

Her er en kodebit som viser hvordan vi kan skrive til en tekstfil ved hjelp av StreamWriter-klassen:

```C#
// Opprette en instans av StreamWriter
StreamWriter sw = new StreamWriter("minTekstfil.txt");

// Skrive tekst til filen
sw.WriteLine("Dette er en tekst som blir lagret i minTekstfil.txt");
sw.WriteLine("Vi kan også legge til variabler og data fra programmet vårt, som for eksempel dette tallet: " + 42);

// Lukke StreamWriter-instansen når vi er ferdige med å skrive
sw.Close();
```

Når vi kjører dette programmet vil det opprette en tekstfil med navnet "minTekstfil.txt" i samme mappe som programmet vårt. Hvis filen allerede eksisterer, vil det nye innholdet bli lagt til i slutten av filen.

##Dypdykk

En viktig ting å merke seg når man skriver til en tekstfil er at vi må spesifisere en filbane. Dette forteller programmet vårt hvor filen skal lagres. Hvis vi ikke gjør dette, vil filen bli lagret i samme mappe som programmet vårt. Det kan også være lurt å sjekke om filen allerede eksisterer før vi skriver til den, for å unngå å overskrive eksisterende data.

Vi kan også bruke StreamWriter-klassen til å appende data til eksisterende filer. Dette betyr at vi kan legge til mer tekst uten å overskrive dataene som allerede er lagret i filen. Dette gjøres ved å spesifisere true som en andre parameter i StreamWriter-klassen sin konstruktør.

Her er et eksempel på hvordan vi kan appende data til en eksisterende tekstfil:

```C#
// Opprette en instans av StreamWriter og appende data
StreamWriter sw = new StreamWriter("minTekstfil.txt", true);

// Append tekst til filen
sw.WriteLine("Dette er mer tekst som blir lagret i minTekstfil.txt");

// Lukke StreamWriter-instansen når vi er ferdige med å skrive
sw.Close();
```

##Se også

- [Microsoft sin dokumentasjon om StreamWriter-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=netframework-4.8)
- [YouTube-video om å skrive til tekstfiler i C#](https://www.youtube.com/watch?v=cuBdl6ujNlU)
- [Artikkel om appende data til tekstfiler i C#](https://www.guru99.com/c-sharp-append-to-file.html)