---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:20.849523-07:00
description: "\xC5 gj\xF8re f\xF8rste bokstav i en streng stor i C# inneb\xE6rer \xE5\
  \ konvertere den f\xF8rste bokstaven i en streng til stor bokstav hvis den ikke\
  \ allerede er det. Denne\u2026"
lastmod: '2024-03-13T22:44:40.777877-06:00'
model: gpt-4-0125-preview
summary: "\xC5 gj\xF8re f\xF8rste bokstav i en streng stor i C# inneb\xE6rer \xE5\
  \ konvertere den f\xF8rste bokstaven i en streng til stor bokstav hvis den ikke\
  \ allerede er det. Denne\u2026"
title: Sette stor bokstav i en streng
weight: 2
---

## Hva & Hvorfor?
Å gjøre første bokstav i en streng stor i C# innebærer å konvertere den første bokstaven i en streng til stor bokstav hvis den ikke allerede er det. Denne endringen kan være avgjørende for formatering av utdata, håndheving av kodingsstandarder eller gjøre tekst i brukergrensesnitt mer lesbart.

## Hvordan:
C# tilbyr en enkel tilnærming til å gjøre strenger store med innebygde metoder. Den enkleste måten å oppnå dette på er ved å direkte endre strengen med disse metodene. For mer komplekse eller spesifikke regler for kapitalisering (f.eks. å gjøre første bokstav stor i hvert ord), kan det være nødvendig med ekstra biblioteker eller manuelle metoder. Nedenfor er eksempler som demonstrerer hvordan du kan gjøre første bokstav i en streng stor på ulike måter i C#.

### Grunnleggende Kapitalisering:
For å gjøre første bokstav i et enkelt ord eller setning stor:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Utdata: "Hello world"
```

### Gjøre Første Bokstav Stor i Hvert Ord:
For å gjøre første bokstav i hvert ord i en streng stor, kan du bruke `TextInfo.ToTitleCase`-metoden funnet i `System.Globalization`-navneområdet:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Utdata: "Hello World"
```

Merk: `ToTitleCase` gjør ikke resten av bokstavene små; den endrer kun den første bokstaven i hvert ord til stor bokstav. Også visse ord i reglene for tittelsaker (som "og", "eller", "av") kan ikke bli gjort store avhengig av kulturinnstillingene.

### Bruk av Utvidelsesmetoder for Gjenbrukbarhet:
Du kan lage en utvidelsesmetode for `string`-klassen for å forenkle prosessen med kapitalisering, noe som gjør koden din renere og mer gjenbrukbar. Slik oppretter og bruker du en slik metode:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Utdata: "Hello world"
    }
}
```

Denne utvidelsesmetoden `Capitalize` kan kalles på ethvert strengobjekt innenfor navneområdet og tilbyr en mer intuitiv og objektorientert tilnærming til strengmanipulasjon i C#.

### Tredjepartsbibliotek:
Selv om C#s standardbibliotek dekker de fleste behov for kapitalisering av strenger, kan visse spesialiserte oppgaver ha nytte av tredjepartsbibliotek, som Humanizer. Imidlertid, for oppgaven med å bare gjøre strenger eller hvert ord i en streng stort, er standard C#-metoder adekvate og effektive, noe som eliminerer behovet for eksterne avhengigheter.
