---
title:                "C#: Sammenføying av strenger"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kombinere eller sammenføye strenger er en viktig del av programmering, spesielt når det kommer til å jobbe med tekst og brukerinputs. Det lar deg enkelt kombinere flere strenger for å lage en lengre og mer meningsfull utgang.

# Hvordan

```C#
 string navn = "Andreas";
 string alder = "25";
 
 // En enkel måte å kombinere strenger på er ved å bruke "+" -operatøren
 string brukerInfo = navn + " er " + alder + " år gammel.";
 Console.WriteLine(brukerInfo);

// Du kan også bruke string.Format () metoden for å kombinere strenger
string brukerInfo2 = string.Format("{0} er {1} år gammel.", navn, alder);
Console.WriteLine(brukerInfo2);

// Resultat:
// Andreas er 25 år gammel.
// Andreas er 25 år gammel.
```

# Dypdykk

Når du kombinerer strenger, kan det også være nyttig å bruke escape-tegn som "/" for å inkludere spesielle tegn som kan forstyrre koden. Det er også viktig å merke seg at å kombinere strenger kan forårsake problemer med ytelsen på applikasjonen din hvis det blir gjort i en løkke. I dette tilfellet bør du heller bruke StringBuilder-klasse, som er spesielt designet for å håndtere store mengder tekst.

# Se også

- [MSDN om string.Concat () metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [MSDN om string.Format () metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)
- [MSDN om StringBuilder-klasse](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)