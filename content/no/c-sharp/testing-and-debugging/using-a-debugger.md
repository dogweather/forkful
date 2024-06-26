---
date: 2024-01-26 03:48:24.443494-07:00
description: "Hvordan: Forestil deg at du har et lite program som ikke oppf\xF8rer\
  \ seg riktig."
lastmod: '2024-03-13T22:44:40.800345-06:00'
model: gpt-4-0125-preview
summary: "Forestil deg at du har et lite program som ikke oppf\xF8rer seg riktig."
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
Forestil deg at du har et lite program som ikke oppfører seg riktig:

```C#
static void Main()
{
    int resultat = Sum(1, 2);
    Console.WriteLine(resultat);
}

static int Sum(int a, int b)
{
    return a + a; // Oops, burde vært a + b
}
```

Ved å bruke Visual Studio sin feilsøker, sett et brytepunkt ved å klikke på venstre marg ved siden av `return a + a;`. Når du kjører programmet (med F5), vil utførelsen pause der. Hold pekeren over variabler for å inspisere verdiene deres, eller bruk Direktevinduet for å evaluere uttrykk. Du vil se at `a` er 1 og `b` er 2, men `a + a` er ikke vår forventede sum. Endre det til `a + b`, fortsett å kjøre (F5), og voilà, konsollen viser 3.

## Dypdykk
Historien om feilsøking går helt tilbake til 1940-tallet da en ekte feil (en møll) ble funnet i en tidlig datamaskin. Dagens feilsøkere, som den i Visual Studio, gir en rekke kraftige funksjoner, inkludert brytepunkter, steg-for-steg utførelse, kunngjøringsvinduer og mer.

Alternativer til Visual Studio sin feilsøker inkluderer åpen kildekode-alternativer som GDB for C-stil språk eller pdb for Python, og plattformuavhengige IDE-er som JetBrains Rider eller VS Code som tilbyr feilsøkingsverktøy for C# og andre språk.

Når du dykker inn i en feilsøkers implementering, ser du på et program som kobler seg til prosessen til applikasjonen din. Den tolker maskinkode, håndterer minnetilstand og kontrollerer kjøringsflyten. Dette er krevende saker som er avgjørende for effektiv feilsøking, noe som er grunnen til at feilsøkingsmodus ofte kjører saktere enn utgivelsesmodus hvor disse krokene ikke eksisterer.

## Se Også
- [Visual Studio Feilsøker Dokumentasjon](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Feilsøkingsstrategier](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
