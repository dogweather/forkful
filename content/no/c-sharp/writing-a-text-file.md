---
title:                "C#: Skrive en tekstfil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det finnes mange grunner til å skrive en tekstfil i et programmeringsspråk som C#. Det kan være nyttig for å lagre data eller for å skrive ut informasjon. Uansett årsak, er det viktig å forstå hvordan man kan skrive og manipulere tekstfiler for å få full nytte av dem.

## Hvordan

For å lage en tekstfil i C#, må vi først opprette et "StreamWriter" objekt som vil tillate oss å skrive til filen. Deretter må vi åpne filen ved å angi filbanen og filnavnet, samt spesifisere om vi vil skrive over en eksisterende fil eller lage en ny fil. Her er et eksempel på hvordan dette kan gjøres:

```C#
StreamWriter skriver = new StreamWriter("minFil.txt", false);

skriver.WriteLine("Dette er en tekstfil som ble skrevet ved hjelp av C#.");
skriver.WriteLine("Du kan også skrive variabler eller data ved hjelp av interpolering.");
skriver.WriteLine($"Datoen i dag er {DateTime.Now}");

skriver.Close();
```

Denne koden vil lage en fil med navnet "minFil.txt" i samme mappe som programmet kjører fra. Det vil deretter skrive tre linjer til filen, og til slutt lukke den for å sikre at dataene er lagret.

Hvis du vil lese fra en eksisterende tekstfil, kan du bruke "StreamReader" objektet på samme måte som "StreamWriter". Her er et eksempel på hvordan du kan lese en fil og skrive ut innholdet på konsollen:

```C#
StreamReader leser = new StreamReader("minFil.txt");

string linje = "";

while ((linje = leser.ReadLine()) != null)
{
    Console.WriteLine(linje);
}

leser.Close();
```

Dette vil lese hver linje fra filen og skrive den ut på konsollen. Igjen, er det viktig å lukke filen når du er ferdig med å lese den.

## Dypdykk

Når du skriver tekstfiler i C#, er det viktig å inkludere "using System.IO" i toppen av koden din. Dette vil importere "System.IO" biblioteket som gir oss tilgang til "StreamReader" og "StreamWriter" objektene.

Det er også viktig å huske på å håndtere eventuelle unntak som kan oppstå når du arbeider med filer, for eksempel hvis filen ikke kan åpnes eller skrives til. Dette kan gjøres ved hjelp av "try-catch" blokker.

I tillegg kan du også konfigurere forskjellige innstillinger for filen din, som å angi kodingen eller endre lese- og skrivetilgangen. Mer informasjon om dette kan bli funnet ved å søke på nettet eller ved å lese dokumentasjonen for "System.IO" biblioteket.

## Se Også

- [Dokumentasjon for "System.IO" biblioteket](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=netcore-3.1)
- [Mer informasjon om å håndtere tekstfiler i C#](https://www.dotnetperls.com/file-reader)
- [Eksempel på bruk av "StreamWriter" og "StreamReader" i C#](https://www.c-sharpcorner.com/uploadfile/mahesh/streamreader-and-streamwriter-in-C-Sharp/)