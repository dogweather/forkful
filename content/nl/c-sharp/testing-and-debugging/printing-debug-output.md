---
title:                "Debug-output afdrukken"
aliases:
- /nl/c-sharp/printing-debug-output/
date:                  2024-01-28T22:04:35.643221-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Debug-output afdrukken gaat over het uitspuwen van informatie die cruciaal is om te begrijpen wat er onder de motorkap van je code gebeurt. Programmeurs doen dit om variabelenwaarden, de stroom van uitvoering te volgen, en om bugs op te sporen—soort van als een spoor van broodkruimels in een digitaal bos.

## Hoe te:
Rechttoe rechtaan spul: gebruik `Console.WriteLine()` om naar de uitvoerconsole te printen. Specifiek voor debugdoeleinden kan `Debug.WriteLine()` je beste keuze zijn, mits je `System.Diagnostics` hebt in je gebruiksdirectieven. Als je een UI-applicatie target, zou `Trace.WriteLine()` de tool voor de taak kunnen zijn, aangezien het luisteraars in staat stelt de uitvoer te vangen.

```C#
using System;
using System.Diagnostics;

public class DebugExample
{
    public static void Main()
    {
        int magischNummer = 42;
        Console.WriteLine("Hallo, mensen! Laten we debuggen.");
        Debug.WriteLine($"Het magische nummer is: {magischNummer}");

        // Doe alsof we hier een voorwaarde hebben
        Trace.WriteLine("We zitten in de matrix!");
    }
}
```

De console-uitvoer zal er zo uitzien:
```
Hallo, mensen! Laten we debuggen.
```

De debug-uitvoer, zichtbaar in het debug-uitvoervenster van je IDE of luisteraar, zal zijn:
```
Het magische nummer is: 42
We zitten in de matrix!
```

## Diepere Duik
Laten we tijdsreizen. Toen C# nieuw was, debugden mensen met berichtvensters—stel je voor dat je honderd keer op 'OK' klikt. Maar gereedschap evolueert. De methode 'Console.WriteLine()' is een betrouwbaar, snelle manier om uitvoer te printen, het best gebruikt in console-apps. Echter, wanneer je verder gaat dan console-apps en bijvoorbeeld Windows Forms of WPF-apps ontwikkelt, worden 'Debug.WriteLine()' en 'Trace.WriteLine()' uit de `System.Diagnostics` namespace aantrekkelijker.

'Debug.Writeline()' geeft alleen uitvoer wanneer de build in Debug-modus is; het is stil in Release-modus. Dit gedrag maakt het netjes voor tijdelijke debug-prints waarover je je later geen zorgen hoeft te maken om op te ruimen. Aan de andere kant kan 'Trace.WriteLine()' zowel voor Debug- als Release-builds ingeschakeld worden, wat kan helpen bij het traceren van problemen na de implementatie.

Het is de moeite waard om op te merken dat `Debug` en `Trace` oproepen door je code heen gestrooid kunnen worden, en je kunt hun uitvoer controleren met behulp van Luisteraars, zonder elke keer opnieuw te hoeven compileren als je verandert waar de uitvoer naartoe gaat. Cool, toch?

## Zie Ook
Voor meer grinniken en kennisbrokken, bekijk deze links:
- De officiële documentatie van Microsoft over `Debug`: [Debug Class (System.Diagnostics)](https://docs.microsoft.com/nl-nl/dotnet/api/system.diagnostics.debug)
- De officiële documentatie van Microsoft over `Trace`: [Trace Class (System.Diagnostics)](https://docs.microsoft.com/nl-nl/dotnet/api/system.diagnostics.trace)
- Een diepe duik in luisteraars en tracebronnen: [Trace Listeners](https://docs.microsoft.com/nl-nl/dotnet/framework/debug-trace-profile/trace-listeners)
