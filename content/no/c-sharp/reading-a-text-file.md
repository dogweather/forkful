---
title:                "C#: Lese en tekstfil"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med datafiler i programmering, har du mest sannsynligvis måttet håndtere tekstfiler. Tekstfiler er en vanlig måte å lagre og håndtere data på, og derfor er det viktig å ha god kunnskap om hvordan man leser og behandler dem. Hvis du ønsker å lære å lese en tekstfil ved hjelp av C# programmeringsspråket, så er du på rett sted.

## Hvordan

Å lese en tekstfil ved hjelp av C# er en relativt enkel prosess som krever noen få linjer med kode. Før vi dykker inn i eksemplene, må vi først forstå noen få konsepter og begreper som er nødvendige for å lese en fil. 

Først og fremst må du forstå at tekstfiler kan inneholde forskjellige typer data og formater, som for eksempel vanlig tekst, tall eller symboler. Derfor må du bestemme hvilken type data du forventer å finne i filen din før du begynner å lese den. 

Et annet viktig konsept er filbanen, som er den unike plasseringen til filen din på datamaskinen din. Det er viktig å ha filbanen riktig formatert i C# koden din for å sikre at programmet finner og kan lese filen.

Nå, la oss se på et eksempel på hvordan du kan lese en tekstfil og skrive ut innholdet til konsollen ved hjelp av C#-kode:

```C#
string filbane = @"C:\Users\Navn\Desktop\tekstfil.txt";

// Opprett et StreamReader-objekt for å lese filen
using(StreamReader sr = new StreamReader(filbane))
{
	// Les hver linje av tekstfilen
	string linje = "";
	while((linje = sr.ReadLine()) != null)
	{
		// Skriv ut hver linje til konsollen
		Console.WriteLine(linje);
	}
}
```

I dette eksemplet bruker vi `StreamReader` -objektet og `ReadLine()` -metoden for å lese hver linje av tekstfilen og skrive dem ut til konsollen.

Et annet nyttig eksempel er å lese en tekstfil og lagre innholdet til en `List` -samling. Dette gjør det enklere å behandle og manipulere teksten senere i koden din.

```C#
string filbane = @"C:\Users\Navn\Desktop\tekstfil.txt";

// Opprett en tom List for å lagre tekstlinjene
List<string> tekstlinjer = new List<string>();

using(StreamReader sr = new StreamReader(filbane))
{
	string linje = "";
	while((linje = sr.ReadLine()) != null)
	{
		// Legg til hver linje i List-samlingen
		tekstlinjer.Add(linje);
	}
}
```

Som du kan se, er det enkelt å lese en tekstfil ved hjelp av C# - du trenger bare å vite hvilke objekter og metoder som er nødvendige for å utføre oppgaven.

## Dypdykk

Som nevnt tidligere, er det viktig å ha riktig forståelse av filbanen og filformatet når du skal lese tekstfiler i C#. Sørg for at du har en grundig forståelse av disse begrepene før du går videre og manipulerer teksten i filen din. 

En annen viktig ting å huske på er at filen din kan inneholde forskjellige tegnsett, som for eksempel Unicode eller ASCII. Du må sørge for at du har riktig koding satt i koden din for å kunne lese og behandle disse forskjellige tegnsettene.

Det er også viktig å ha en god feilhåndtering når du leser tekstfiler. Dette sikrer at programmet ditt håndterer eventuelle unntak eller problemer som kan oppstå under lesing av filen.

## Se også

- [Offisiell dokumentasjon for StreamReader klasse](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# StreamReader Tutorial](https://www.c-sharpcor