---
title:    "Elixir: Oppretting av en midlertidig fil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Hvorfor

Å opprette midlertidige filer er en vanlig oppgave i programmering. Disse filene brukes ofte til å lagre midlertidig data som bare vil være relevant for en kort periode. Dette kan inkludere midlertidige lagringssteder for nedlastede filer, midlertidige datagenereringer eller midlertidige bufferområder for prosesser. I Elixir vil du ofte møte behovet for å opprette midlertidige filer når du jobber med dataoverføring, filbehandling og webskraping.

##Slik gjør du det

Å opprette en midlertidig fil i Elixir er en enkel prosess ved hjelp av standardbiblioteket. Først må du importere `File`-modulen ved å inkludere `:file` i modulen. Deretter kan du bruke `tempfile/2`-funksjonen til å opprette en midlertidig fil. Denne funksjonen tar inn et filnavn og en liste over alternativer som argumenter. Her er et eksempel på hvordan du kan bruke `tempfile/2`-funksjonen:

```
import File
{tempfile, filepath} = tempfile("my_temp_file.txt", [:temp_dir, "path/to/temp/dir/"])
```

I dette eksempelet opprettes en midlertidig fil med navnet "my_temp_file.txt" i mappen "path/to/temp/dir/". Merk at du også kan legge til andre alternativer som `[:prefix, "temp_"]` for å gi filnavnet et prefiks, eller `[:suffix, "_temp"]` for å legge til et suffiks. Når filen er opprettet, returnerer funksjonen en tuple med filobjektet og stien til filen.

For å lese og skrive til den midlertidige filen, kan du bruke `File.read/1` og `File.write/2`-funksjonene. Her er et eksempel på hvordan du kan lese fra og skrive til den midlertidige filen:

```
File.read(filepath)
|> Enum.each(fn line -> IO.puts(line) end)

File.write(filepath, "Dette er en midlertidig fil")
```

Når du er ferdig med å bruke den midlertidige filen, må du slette den ved å bruke `File.rm/1`-funksjonen. Dette er viktig for å unngå å fylle opp disken med unødvendige midlertidige filer.

##Dypdykk

I Elixir kan du også bruke `tempfile/3`-funksjonen for å opprette en midlertidig fil i en spesifikk mappe og med et bestemt prefiks og suffiks. Du kan også bruke `tempfile!/2`-funksjonen hvis du vil få en feilmelding hvis filen ikke kan opprettes. Utover dette har Elixir også mange andre nyttige funksjoner for filbehandling, inkludert `File.stat/1` for å få informasjon om en fil og `File.rename/2` for å endre navnet på en fil.

##Se også

- [Elixir File modul dokumentasjon](https://hexdocs.pm/elixir/File.html)
- [Elixir Tempfile prosjekt på Github](https://github.com/lsm/elixir-tempfile)