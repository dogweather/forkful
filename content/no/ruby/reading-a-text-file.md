---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil er prosessen med å få innholdet i en fil til programmet ditt. Programmerere gjør dette for å manipulere data, utføre filbaserte operasjoner eller hente informasjon lagret i tekstformat. 

## Hvordan gjøre det:
La oss se hvordan vi kan lese en tekstfil i Ruby. Vi bruker Ruby's innebygde `File`-klasse.

```Ruby
fil = File.open("eksempel.txt", "r")
innhold = fil.read
puts innhold
fil.close
```

Når vi kjører koden over, vil den lese innholdet i "eksempel.txt" og skrive det ut på skjermen. 

## Dyp Dykk
Historisk sett har lesing av tekstfiler vært en grunnleggende operasjon i programmering, helt siden tidlig databehandling. Alternativt til 'File' klassen, tilbyr Ruby også 'IO' og 'IOBin' klassene for mer spesifikke behov. Tekstfil lesing er implementert i Ruby via en underliggende C API, som gjør operasjonen rask og effektiv. Du kan kontrollere nøyaktig hvordan du vil lese filen, inkludert hvilken buffering strategi du vil bruke, ved å endre 'File.open' modusparameter.

## Se Også
For mer informasjon om filhåndtering i Ruby, sjekk ut disse nyttige lenkene:
- [Detaljert tutorial om filhåndtering i Ruby](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)