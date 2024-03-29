---
date: 2024-01-20 17:38:08.021323-07:00
description: "Konvertering av en streng til sm\xE5 bokstaver endrer alle bokstavene\
  \ i strengen til deres sm\xE5bokstav motstykke. Dette gj\xF8res for konsistens i\u2026"
lastmod: '2024-03-13T22:44:40.782095-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av en streng til sm\xE5 bokstaver endrer alle bokstavene i\
  \ strengen til deres sm\xE5bokstav motstykke. Dette gj\xF8res for konsistens i\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver endrer alle bokstavene i strengen til deres småbokstav motstykke. Dette gjøres for konsistens i sammenligninger og søk, hvor store og små bokstaver skal behandles likt.

## Hvordan gjøre det:
C# har innebygget støtte for å endre store bokstaver til små bokstaver. Her er et raskt eksempel:

```C#
string original = "Hei, VERDEN!";
string smallCaps = original.ToLower();

Console.WriteLine(smallCaps);
```

Kjører du dette, får du følgende output:

```
hei, verden!
```

## Dypdykk
Konvertering til små bokstaver har eksistert i programmeringsspråk lenge, som en standardstring-operasjon. Det brukes ofte i behandling av tekst der bokstavstørrelsen ikke skal påvirke resultatet, som når du sammenligner epostadresser eller brukernavn.

I C# gjøres dette med `ToLower()` eller `ToLowerInvariant()`. Forskjellen er at `ToLowerInvariant()` ignorerer kulturelle forskjeller og gir en mer uniform result, mens `ToLower()` bruker den gjeldende kulturelle konteksten. For eksempel, i tyrkisk er det to forskjellige små bokstaver for 'i' – én med prikk og én uten.

Det er også alternativ metoder som `ToLowerCulture()`, men `ToLower()` og `ToLowerInvariant()` er de mest vanlige å bruke.

Implementasjonsdetaljer kan avhenge av programmeringsspråket og den underliggende strukturen. I .NET-plattformen, der C# lever, bruker `ToLower()` og `ToLowerInvariant()` Unicode-standard for karaktertransformasjon.

## Se Også
- Microsofts dokumentasjon på `String.ToLower()`: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- Unicode-standard for casemapping: https://www.unicode.org/reports/tr21/tr21-5.html
- `String.ToLowerInvariant()` på Microsoft Docs: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant
