---
title:                "Leser kommandolinjeargumenter"
html_title:           "C#: Leser kommandolinjeargumenter"
simple_title:         "Leser kommandolinjeargumenter"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kommandolinje argumenter lesing er når et program mottar og behandler parameterne som brukes for å kjøre det fra kommandolinjen. Dette er nyttig fordi det lar programmene være mer fleksible og brukervennlige, og tillater brukerne å tilpasse hvordan programmet kjører ved å gi forskjellige argumenter.

## Hvordan:
```C#
using System;

class Program
{
	static void Main(string[] args)
	{
		Console.WriteLine("Hei, " + args[0] + "!"); // For eksempel, hvis "Hei, verden!" er skrevet på kommandolinjen, vil utskriften være "Hei, verden!" 
	}
}

// Kommandolinjen argumenter kan også være numeriske verdier, og kan brukes for å utføre forskjellige handlinger i programmet basert på disse verdiene.
```

## Dykk dypere:
Kommandolinje argumenter lesing har eksistert siden starten av operativsystemer, og er fortsatt en svært vanlig praksis i utviklingen av programmer. En alternativ måte å få inndata til et program på er ved å bruke standardinndata, men dette kan være mindre fleksibelt enn å lese kommandolinjen argumenter. I C#, er det flere biblioteker og metoder som kan brukes til å forenkle lesingen av argumentene, for eksempel Environment.GetCommandLineArgs() metoden.

## Se også:
- [Microsofts dokumnetasjon for kommandolinje argumenter i C#](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netframework-4.8)
- [En guide for å lese kommandolinje argumenter i forskjellige programmeringsspråk](https://www.mysysadmintips.com/windows/other/524-get-command-line-arguments-in-c-cpython-and-ruby)