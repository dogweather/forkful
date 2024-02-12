---
title:                "Interpolering av en streng"
aliases:
- /no/c-sharp/interpolating-a-string/
date:                  2024-01-20T17:50:23.349565-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Stringinterpolasjon lar deg plugge variabler og uttrykk rett inn i tekststrenger. Det forenkler både koden og gjør det mer leselig, ved å slippe å bruke en haug av `+` teikn for å sette sammen strenger.

## How to:

Med interpolasjon bruker vi `{}` for å inkludere variabler inne i en streng, forhåndet av `$`:

```C#
string navn = "Ola";
int alder = 25;
string beskrivelse = $"Hei, jeg heter {navn} og jeg er {alder} år gammel.";
Console.WriteLine(beskrivelse);
```

Sample output:
```
Hei, jeg heter Ola og jeg er 25 år gammel.
```

## Deep Dive

Før C# 6.0 brukte vi `String.Format` for å sette inn verdier i en streng, som så mer klønete ut. Etter C# 6.0 er interpolasjon standarden, da det både er enklere og raskere. Under panseret bruker C# en `FormattableString` klasse for å gjøre om kode med interpolasjon til kjørbar kode, og i kjøretid behandlet like som `String.Format`. Det finnes alternativer, som konkatenasjon (`+` operator) eller `StringBuilder`, men de anbefales for spesifikke tilfeller, ikke generelt.

## See Also

Her er enda mer lesestoff:

- Microsofts offisielle dokumentasjon: [String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- Et dypdykk i `FormattableString`: [FormattableString Class](https://docs.microsoft.com/en-us/dotnet/api/system.formattablestring?view=net-6.0)
- Når du bør bruke `StringBuilder`: [When to use StringBuilder](https://docs.microsoft.com/en-us/dotnet/standard/base-types/stringbuilder)
