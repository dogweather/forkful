---
title:                "Lesing av en tekstdokument"
html_title:           "C#: Lesing av en tekstdokument"
simple_title:         "Lesing av en tekstdokument"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å lese en tekstfil i C# betyr å åpne og lese informasjon fra en tekstfil på datamaskinen din. Programmere gjør dette for å kunne bruke data som er lagret i en tekstfil i sine programmer.

# Hvordan:
Det er enkelt å lese en tekstfil i C#. Du kan bruke ```File.ReadAllText()``` metoden for å lese innholdet fra en tekstfil som en enkelt streng, eller ```File.ReadAllLines()``` metoden for å lese innholdet i tekstfiler som en matrix av separate linjer. Se eksempler nedenfor.

```C#
// Eksempel 1: Leser en tekstfil som en enkelt streng
string tekst = File.ReadAllText("minTekstfil.txt");
Console.WriteLine(tekst);

// Eksempel 2: Leser en tekstfil som en matrix av linjer
string[] linjer = File.ReadAllLines("minTekstfil.txt");
foreach (string linje in linjer)
{
    Console.WriteLine(linje);
}

/* Output
Eksempeltekst på første linje
Dette er en annen linje med tekst
Siste linje med litt mer tekst 
*/

```

# Dypdykk:
Å lese og skrive til tekstfiler har vært et essensielt aspekt av programmering siden begynnelsen. Tekstfiler brukes ofte for å lagre landealder, konfigurasjoner, tekst og andre typer data i et lett-å-lese format. Noen alternativer til å bruke ```File``` klassen for å lese tekstfiler inkluderer å bruke andre klasser som ```StreamReader``` eller tredjepartsbiblioteker som ```CsvHelper```. Når du leser tekstfiler i C#, er det også viktig å tenke på formatering, tegnkoding, og eventuell feilhåndtering.

# Se også:
- [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext?view=net-5.0)
- [StreamReader klasse](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [CsvHelper bibliotek](https://joshclose.github.io/CsvHelper/)