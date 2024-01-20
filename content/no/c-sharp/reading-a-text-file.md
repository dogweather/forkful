---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

---
# Lesere tekstfil i C#: En veiledning

## Hva & Hvorfor?

Lesing av en tekstfil er en operasjon som tillater programmerere å få tilgang til og manipulere data lagret i tekstfilformat. Det er en avgjørende oppgave i mange programmeringsscenarioer som dataanalyse, loggføring, konfigurasjoner, etc.

## Hvordan gjøre det:

Her er et enkelt eksempel på lesing av en tekstfil i C# ved hjelp av `System.IO`-biblioteket.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
		string txtfil = File.ReadAllText(@"C:\Eksempel.txt");
		Console.WriteLine(txtfil);
    }
}
```

Forventet output:

```
Hallo verden!
Dette er en prøvetekst.
```

## Dypdykk

**Historisk kontekst:** Innlesing av tekstfiler er en funsjonalitet som har blitt implementert i mange programmeringsspråk lenge før C#. Dette understreker dens viktighet i ulike programmeringsapplikasjoner.

**Alternativer:** Det finnes flere måter å lese tekstfiler på i C#. Du kan for eksempel bruke `StreamReader`-klassen eller `File.ReadLines()`-metoden for å lese filer linje for linje, noe som er mer minneeffektivt for store filer.

**Implementeringsdetaljer:** `File.ReadAllText()` i eksempelet ovenfor laster hele filen inn i minnet. Dette er greit for små til mellomstore filer, men for større filer anbefales det å bruke en mer minneeffektiv metode, som `File.ReadLines()` som behandler filen linje for linje.

## Se også

2. StackOverflow: [How to read a large text file line by line using Java?](https://stackoverflow.com/questions/5868369/how-to-read-a-large-text-file-line-by-line-using-java)
3. Microsoft Docs: [Streamreader Class (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
---