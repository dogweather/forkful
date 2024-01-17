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

Hva & Hvorfor? 
Å opprette en midlertidig fil er en vanlig praksis blant programmerere når du trenger å lagre midlertidig data eller behandle store mengder data. Dette gjøres vanligvis for å optimalisere og effektivisere koden, og for å sikre at den riktige informasjonen blir lagret og behandlet.

Hvordan skrive kode for å opprette en midlertidig fil: 
For å opprette en midlertidig fil kan du bruke "System.IO.Path.GetTempFileName()" metoden i C#. Denne metoden vil returnere en streng som representerer stien til den midlertidige filen som er laget. Du kan også bruke "System.IO.Path.GetTempPath()" for å få tilgang til mappen som inneholder alle midlertidige filer.

```C#
string tempFile = System.IO.Path.GetTempFileName();
Console.WriteLine("Midlertidig filsti: " + tempFile);
```

Deep Dive: 
Historisk sett har midlertidige filer blitt brukt for å lagre midlertidig data på disken for å frigjøre minne og optimalisere ytelsen til et program. I dag brukes det også til å håndtere store mengder data og for å sikre at ingen kritisk informasjon går tapt i tilfelle programmet krasjer.

Alternativer til å opprette en midlertidig fil inkluderer å opprette en fil i en midlertidig mappe eller å bruke "System.IO.File.CreateTempFile()". Implementasjonsdetaljer om oppretting av midlertidige filer kan variere fra plattform til plattform.

Se også: 
For mer informasjon om bruk av midlertidige filer i C#, kan du lese mer på Microsofts offisielle dokumentasjonsside for "System.IO.Path.GetTempFileName()": https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename