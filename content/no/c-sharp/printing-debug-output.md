---
title:                "C#: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Utskrift av feilsøkingsmeldinger er en viktig del av utviklingsprosessen. Det hjelper deg med å identifisere og løse problemer i koden din, og kan bidra til å forbedre kvaliteten på programvaren din.

## Slik gjør du det

For å skrive ut feilsøkingsmeldinger i C#, kan du bruke Console.WriteLine() -funksjonen. Dette vil skrive ut informasjonen du ønsker å vise i konsollen.

```C#
Console.WriteLine("Dette er en feilsøkingsmelding");
```

Output:
`Dette er en feilsøkingsmelding`

Du kan også inkludere variabler i meldingen ved å bruke `+` -tegnet for å kombinere dem med teksten.

```C#
string navn = "Per";
Console.WriteLine("Hei, mitt navn er " + navn);
```

Output:
`Hei, mitt navn er Per`

Du kan også bruke string interpolation for å gjøre koden din mer leselig.

```C#
string navn = "Per";
Console.WriteLine($"Hei, mitt navn er {navn}");
```

Output:
`Hei, mitt navn er Per`

## Dypdykk

Du kan også formatere utskriften din ved hjelp av spesifikke formateringskoder. For eksempel, for å vise et nummer med to desimaler, kan du bruke `F2` formatkoden.

```C#
decimal pris = 123.456;

Console.WriteLine("Totalkostnad: {0:F2}", pris);
```

Output:
`Totalkostnad: 123.46`

Det er også mulig å styre farge, størrelse og annen formatering av teksten din ved hjelp av konsollkommandoen `Console.ForegroundColor` og `Console.BackgroundColor`.

## Se også

- [C# Console.WriteLine() - Offisiell dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.console.writeline?view=net-5.0)
- [Feilsøking i C# - Enkel guide](https://www.tutorialspoint.com/csharp/csharp_debugging.htm) 
- [Skriver ut til konsollen i C#](https://www.geeksforgeeks.org/how-to-print-in-console-in-c-sharp/)