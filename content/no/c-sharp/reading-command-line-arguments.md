---
title:    "C#: Lesing av kommandolinje-argumenter"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er en viktig del av programmering i C#. Dette lar deg gi innspill til programmene dine så det kan gjøre forskjellige ting basert på det brukeren skriver inn.

## Hvordan

Å lese kommandolinje-argumenter i C# er enkelt og kan være veldig nyttig. Først, må du legge til System Namespace i prosjektet ditt. Deretter kan du bruke følgende eksempelkode for å lese inn argumenter:

```C#
static void Main(string[] args)
{
    // Les inn alle argumenter gitt av brukeren
    foreach (string arg i args)
    {
        Console.WriteLine(arg);
        // Her kan du legge til logikk som skal utføres basert på argumentene
    }
}
```

Når du kjører programmet og gir det argumenter, vil du se at de blir skrevet ut til konsollen. For eksempel, hvis du gir programmet ditt to argumenter, "tekst" og "123", vil utgangen være:

```
tekst
123
```

Dette gjør det mulig for deg å tilpasse funksjonaliteten til programmet ditt basert på hva brukeren skriver inn.

## Dypdykk

Det er også mulig å håndtere forskjellige typer argumenter, som for eksempel tall eller boolske verdier. Dette kan gjøres ved å bruke Convert-klassen for å konvertere argumentene dine til ønsket datatype.

For eksempel, hvis du vil konvertere det første argumentet til et heltall, kan du bruke følgende kode:

```C#
int arg1 = Convert.ToInt32(args[0]);
```

Dette lar deg bruke argumentet som et heltall i logikken din. Du kan også håndtere feil ved å bruke try-catch-konstruksjonen for å unngå at programmet ditt krasjer hvis en bruker skriver inn noe annet enn det som er forventet.

## Se Også

- [Microsoft sin dokumentasjon om å lese kommandolinje-argumenter i C#](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netcore-3.1)
- [Eksempler på hvordan man kan håndtere kommandolinje-argumenter i C#](https://www.geeksforgeeks.org/command-line-arguments-in-c-sharp/)
- [En guide til konvertering av argumenter i C#](https://www.c-sharpcorner.com/article/c-sharp-programming-how-to-convert-command-line-arguments/)