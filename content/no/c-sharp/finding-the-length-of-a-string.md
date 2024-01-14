---
title:                "C#: Å finne lengden på en streng"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Blogginnlegg om programmering i C# for casual lesere

##Hvorfor
Å finne lengden på en tekststreng er en vanlig oppgave i programmering og kan være nyttig for å manipulere data eller vise informasjon til brukere. I dette blogginnlegget vil vi se nærmere på hvordan vi kan finne lengden på en streng i C#.

##Slik gjør du det
```C#
// Definerer en tekststreng
string tekst = "Dette er en tekststreng.";

// Bruker .Length attributtet for å finne lengden
Console.WriteLine("Lengden av tekststrengen er: " + tekst.Length);

//Output: Lengden av tekststrengen er: 24
```

Vi kan også bruke en løkke til å telle antall tegn i en tekststreng:

```C#
// Definerer en tekststreng
string tekst = "Dette er en tekststreng.";

// Definerer en variabel for å holde tellingen
int lengde = 0;

// Løkke som teller antall tegn
for (int i = 0; i < tekst.Length; i++)
{
    lengde++;
}

Console.WriteLine("Lengden av tekststrengen er: " + lengde);

//Output: Lengden av tekststrengen er: 24
```

Vi kan også bruke metoden `.Count()` for å finne lengden på en tekststreng. Denne metoden tar inn et betinget uttrykk som argument og teller hvor mange ganger uttrykket er oppfylt i tekststrengen.

```C#
// Definerer en tekststreng
string tekst = "Dette er en tekststreng.";

// Bruker .Count() metoden
Console.WriteLine("Lengden av tekststrengen er: " + tekst.Count());

//Output: Lengden av tekststrengen er: 24
```

##Dypdykk
Hvis vi ønsker å ekskludere mellomrom fra tellingen av tegn, kan vi bruke `.Trim()` metoden i kombinasjon med `.Length` attributtet.

```C#
// Definerer en tekststreng
string tekst = "Dette er en tekststreng.";

// Bruker .Trim() metoden for å fjerne mellomrom
int lengde = tekst.Trim().Length;

Console.WriteLine("Lengden av tekststrengen uten mellomrom er: " + lengde);

//Output: Lengden av tekststrengen uten mellomrom er: 20
```

Det kan også være nyttig å vite at `.Length` attributtet returnerer en `int` verdi, altså et heltall. Hvis vi ønsker å bruke lengden i en matematisk operasjon, kan det være nødvendig å konvertere det til et desimaltall ved å bruke `Convert.ToDouble()`.

```C#
// Definerer en tekststreng
string tekst = "Dette er en tekststreng.";

// Konverterer lengden til et desimaltall
double lengde = Convert.ToDouble(tekst.Length);

// Ganger lengden med 2
double dobbelLengde = lengde * 2;

Console.WriteLine("Dobbelt så lang tekststreng: " + dobbelLengde);

//Output: Dobbelt så lang tekststreng: 48
```

##Se også
[Microsoft Docs om String.Length Eigenskap](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)\
[Microsoft Docs om String.Count Metode](https://docs.microsoft.com/en-us/dotnet/api/system.string.count?view=net-5.0)\
[Tutorialspoint om C# Strings](https://www.tutorialspoint.com/csharp/csharp_strings.htm)