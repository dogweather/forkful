---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av feilsøkingsdata er en teknikk for å spore flyten av en programkode. Dette gjør programmerere fordi det lar dem se tydelig hva som skjer på hvert trinn av programmet, noe som gjør det lettere å feilsøke.

## Slik gjør du:

Her er en enkel C# eksempelkode som viser hvordan man kan skrive ut debugging.

```C#
using System.Diagnostics;

class Program
{
    static void Main()
    {
        Debug.WriteLine("Dette er en debug melding");
    }
}
```

Når du kjører dette koden, vil den skrive ut "Dette er en debug melding" i feilsøkingskonsollen.

## Dyp Dykk

Historisk sett har utskrift av feilsøkingsdata vært en grunnleggende del av programmering siden de første datamaskinene. Alternativer til utskrift inkluderer bruk av debuggere, men disse er ofte mer komplekse og tidkrevende å sette opp.

I C# bruker vi `System.Diagnostics.Debug`-klassen for å skrive ut debugmeldinger. Denne klassen har forskjellige metoder, men `WriteLine` er kanskje den mest brukte. Vær oppmerksom på at du må kjøre koden din i debug-modus for å se disse meldingene. De vises ikke når koden kjøres i release-modus.

## Se også:

1. [System.Diagnostics.Debug Class Dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0)
2. [Debugging i Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)
3. [Feilsøkingstips og triks i C#](https://stackify.com/csharp-debugging-tips/)