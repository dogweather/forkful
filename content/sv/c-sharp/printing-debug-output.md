---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Utskrift av felsökningsoutput är metoden för att visa värden, mätningar, eller tillstånd som är användbara för att kunna felsöka koden under utveckling. Programmerare gör detta för att underlätta identifiering och lösning av problem.

## Hur till:

För att skriva ut felsökningsoutput i C#, kan du använda System.Diagnostics.Debug-klassen. Exempel nedan:
```C#
System.Diagnostics.Debug.WriteLine("Ditt meddelande här");
```

Om du vill skicka med variabelvärden kan du göra det med `string.Format` eller interpolerade strängar.
```C#
string variabel = "Exempelvärde";
System.Diagnostics.Debug.WriteLine($"Värdet på variabeln är {variabel}.");
```
Detta kommer att generera följande utdata:
```C#
Värdet på variabeln är Exempelvärde.
```

## Djupdykning

Utskrift av felsökningsoutput har länge varit en standardmetod för att felsöka datorprogram, speciellt under utvecklingsstadiet. I C#, implementeras detta med System.Diagnostics.Debug klassen, men det finns alternativ. 

Ett alternativ är `System.Console.WriteLine`, som skriver till standardutdata i stället för felsökningsinstallationen. Ett annat är `System.Diagnostics.Trace`, som skriver både till felsöknings- och utsläppskonsolerna.

När du använder `System.Diagnostics.Debug`, skrivs meddelanden endast ut när koden kompileras med `DEBUG`-symbolen. Detta innebär att felsökningsutskrifter inte kommer att visas i produktionen, vilket hjälper till att optimera prestanda och minska informationens exponering.

## Se även

- MSDN-dokumentation om System.Diagnostics.Debug: [Här](https://docs.microsoft.com/sv-se/dotnet/api/system.diagnostics.debug?view=net-5.0)
- En grundlig jämförelse mellan `Debug` och `Trace`: [Här](https://stackoverflow.com/questions/519233/writing-to-output-window-of-visual-studio)
- Microsofts guide om felsökning i Visual Studio: [Här](https://docs.microsoft.com/sv-se/visualstudio/debugger/?view=vs-2019)