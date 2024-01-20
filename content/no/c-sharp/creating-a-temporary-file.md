---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---
# Opprette midlertidige filer i C#

## Hva & Hvorfor?

Å lage en midlertidig fil betyr å lage en fil som bare eksisterer for kort tid, oftest til programmet er ferdig kjørt. Programmers gjør dette til midlertidig lagring og manipulering av data uten å belaste hovedminnet.

## Hvordan:

For å opprette en temprær fil bruker vi metoden `Path.GetTempFileName()` i `System.IO`-namespacet.
Her er koden:


```C#
using System.IO;

class Demo
{
    static void Main()
    {
        string tempFile = Path.GetTempFileName();

        using (StreamWriter sw = new StreamWriter(tempFile))
        {
            sw.WriteLine("Heldigvis er jeg en midlertidig fil.");
            sw.WriteLine("Når dette programmet er ferdig kjørt, forsvinner jeg.");
        }

        using (StreamReader sr = new StreamReader(tempFile))
        {
            string line;

            while ((line = sr.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```
Sample output:
```
Heldigvis er jeg en midlertidig fil.
Når dette programmet er ferdig kjørt, forsvinner jeg.
```

## Dyp Dykk

Historisk sett, har midlertidige filer vært brukt i programmering helt siden de tidlige dagene av batchbehandling for å lagre mellomresultater av en lang oppgave.

Et alternativ til å bruke midlertidige filer er databaser. Noen programmer kan dra fordeler av å bruke SQLite eller lignende teknologier på grunn av deres evne til å håndtere dataene mer effektivt.

I løpet av implementasjonen er det viktig å være oppmerksom på sikker riktig bruk og rengjøring av de midlertidige filene. Ineffektiv håndtering av midlertidige filer kan føre til sikkerhetsproblemer eller unødvendig disklagring.

## Se også

* [Offisiell dokumentasjon av 'Path.GetTempFileName'](https://docs.microsoft.com/dotnet/api/system.io.path.gettempfilename)
* [Alternativer til midlertidige filer](https://devblogs.microsoft.com/oldnewthing/?p=3923)
* [Ytterligere informasjon om filhåndteringsfunksjoner i C#](https://docs.microsoft.com/dotnet/api/system.io.file)