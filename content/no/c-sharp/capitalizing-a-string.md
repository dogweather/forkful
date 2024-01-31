---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I C# er det å kapitalisere en streng å endre dens første bokstav til en stor bokstav. Det brukes for å standardisere tekstdata, forbedre lesbarheten, eller tilpasse seg stil- og formatteringsregler.

## Hvordan:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string original = "oslo er kult.";
        string capitalized = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(original);

        Console.WriteLine(capitalized); // Output: Oslo Er Kult.
    }
}
```
Her behandler `ToTitleCase` hvert ord. For bare første bokstav:
```C#
string original = "oslo";
string capitalized = original[0].ToString().ToUpper() + original.Substring(1);

Console.WriteLine(capitalized); // Output: Oslo
```

## Dypdykk
Kapitalisering av strenger i programmering har eksistert like lenge som behovet for tekstbehandling. Metoder og funksjoner for å gjøre dette har utviklet seg gjennom språk og standardbiblioteker.

I C# er `ToTitleCase` fra `TextInfo` klassen i `System.Globalization` namespace ofte brukt, men husk at den gjør mer enn å bare kapitalisere første bokstav - den påvirker hele strengen. Noen kulturer har ikke konseptet med store bokstaver, så pass på kulturell sensitivitet ved internasjonalisering.

En enklere metode er å bruke `ToUpper` på den første bokstaven og så legge til resten av strengen. Det er direkte og kontrollerbart, men ikke internasjonaliserbart. `ToTitleCase` har sine quirks, for eksempel vil den ikke endre allerede store bokstaver, så "iOS" blir til "IOS".

Et alternativ er å bruke `char.ToUpper` hvis du bare vil behandle én bokstav. `ToUpperInvariant` er nyttig når du vil at den kapitaliserte strengen skal se lik ut uansett kulturell kontekst.

## Se Også:
- [Microsoft Docs: TextInfo.ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
- [Microsoft Docs: String.ToUpperInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
- [Stack Overflow: Capitalize First Character of each word](https://stackoverflow.com/questions/4135317/make-first-letter-of-a-string-upper-case-with-maximum-performance) for diskusjoner og ytterligere løsninger.
