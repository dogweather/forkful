---
title:    "C#: Å skrive en tekstfil"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være nyttig i mange sammenhenger innen programmering. Det kan være et enkelt og effektivt verktøy for å lagre og organisere data, eller for å opprette enkel tekstbasert output. Å kunne skrive en tekstfil er en viktig ferdighet for enhver utvikler.

## Hvordan

For å skrive en tekstfil i C#, må vi bruke System.IO biblioteket. Først må vi opprette en instans av StreamWriter-klassen for å kunne skrive til filen. Deretter kan vi bruke metoden `WriteLine()` for å skrive tekst til filen. Til slutt må vi lukke filen ved å bruke `Close()`-metoden.

```C#
using System.IO;

string filnavn = "minTekstfil.txt";

// Oppretter en instans av StreamWriter og åpner filen for skriving
StreamWriter sw = new StreamWriter(filnavn);

// Skriver tekst til filen
sw.WriteLine("Dette er en tekstfil som er skrevet i C#");
sw.WriteLine("Den kan inneholde flere linjer med tekst");

// Lukker filen
sw.Close();
```

For å lese innholdet i tekstfilen kan vi bruke StreamReader-klassen på samme måte som vi brukte StreamWriter. Vi må først opprette en instans av klassen og åpne filen ved hjelp av `OpenRead()`-metoden. Deretter kan vi bruke `ReadLine()`-metoden for å lese linje for linje fra filen.

```C#
// Oppretter en instans av StreamReader og åpner filen for lesing
StreamReader sr = new StreamReader(filnavn);

// Leser innholdet i filen linje for linje
string linje = sr.ReadLine();
while (linje != null)
{
    Console.WriteLine(linje);
    linje = sr.ReadLine();
}

// Lukker filen
sr.Close();
```

Dette er en enkel måte å skrive og lese fra en tekstfil i C#, men det finnes også andre metoder og teknikker. Les gjerne mer om System.IO biblioteket for å utforske flere muligheter.

## Dypdykk

Å skrive en tekstfil er ikke bare nyttig for å lagre og lese data, men det kan også være nyttig for å generere rapporter eller loggfiler. Vi kan for eksempel bruke StreamWriter til å skrive ut informasjon om et program mens det kjører, og dermed få en oversikt over hva som skjer.

En annen nyttig funksjon ved å skrive en tekstfil er at den er lesbar både for mennesker og maskiner. Dette gjør at dataene kan deles og behandles enklere av ulike programmer.

## Se også

- [Microsoft Docs - System.IO Namespace](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=net-5.0)
- [C# FileStream Class](https://www.w3schools.com/cs/cs_filestreams.asp)
- [C# Read and Write Files (Tutorial)](https://www.c-sharpcorner.com/blogs/working-with-text-files-in-c-sharp1)