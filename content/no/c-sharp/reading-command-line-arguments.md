---
title:                "C#: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å lese kommandolinje-argumenter er en nyttig og effektiv måte å gi programmet ditt ekstra funksjonalitet på. Ved å lese disse argumentene kan du gi brukerne dine muligheten til å tilpasse programmet ditt og gi dem en bedre opplevelse.

## Hvordan du gjør det
For å lese kommandolinje-argumenter i C# kan du bruke `string[] args` parameteret i `Main()` metoden. Dette vil få programmet ditt til å lese alle argumentene som blir angitt når det kjøres.

```C#
static void Main(string[] args)
{
    // Sjekker om det er angitt noen argumenter
    if (args.Length > 0)
    {
        // Går igjennom alle argumentene og skriver dem ut
        foreach (string argument in args)
        {
            Console.WriteLine(argument);
        }
    }
    else
    {
        Console.WriteLine("Ingen argumenter angitt.");
    }
}
```

Lar oss si at du har et program kalt "hello.exe" og kjører det i kommandolinjen med argumentene "Bonjour" og "Hei". Resultatet vil bli:

```
Bonjour
Hei
```

Som du kan se, vil alle kommandolinje-argumentene bli lagret som strenger i `args` arrayet. Du kan også gjøre om argumentene til andre datatyper, som for eksempel ved å bruke `int.Parse()` eller `bool.Parse()`.

## Dypdykk
I tillegg til å lese enkeltargumenter, kan du også tolke kombinasjoner av argumenter. For eksempel kan du lage en kommando for å få programmet ditt til å gjøre forskjellige ting avhengig av hvilke argumenter som blir oppgitt.

Du kan også implementere feilhåndtering for å sikre at programmet ikke krasjer hvis de oppgitte argumentene er ugyldige eller mangler.

## Se også
- [Offisiell C# dokumentasjon om kommandolinje-argumenter](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [En guide til kommandolinje-argumenter i C#](https://www.dreamincode.net/forums/topic/216103-command-line-arguments-in-c%23/)
- [En steg-for-steg veiledning til å behandle kommandolinje-argumenter i C#](https://docs.microsoft.com/en-us/tonywhites/how-to-provide-command-line-arguments-in-visual-studio)