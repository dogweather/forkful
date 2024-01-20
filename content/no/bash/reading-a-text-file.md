---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er prosessen med å hente informasjon lagret i en fil i klar tekstformat. Programmerere gjør dette for å bruke eller manipulere data lagret i filen.

## Hvordan:

Tekstfiler kan leses på flere måter i Bash, men den enkleste og mest vanlige metoden er å bruke `cat`, `more`, eller `less`. 

For eksempel:
```Bash
cat filnavn.txt
```
denne kommandoen vil vise alt innholdet i filen i konsollen.

Hvis filen inneholder mer data enn skjermen kan vise, kan `more` eller `less` være mer passende.
```Bash
more filnavn.txt
less filnavn.txt
```
Disse kommandoene lar deg bla gjennom filinnholdet side for side. 

## Dypdykk:

`cat`-kommandoen har vært en del av Unix- og Linux-baserte systemer siden 70-tallet. Alternativt, for større filer, anbefales 'more' eller 'less' på grunn av deres evne til å bla gjennom filen.

I tillegg, for mer komplekse operasjoner, tilbyr `awk` og `sed` scripting-funksjonalitet for tekstmanipulasjon. Bruk av disse verktøyene går utenfor omfanget av denne artikkelen, men de er verdt å utforske for omfattende tekstbehandling.

Merk at å lese en fil direkte i Bash bare er passende for mindre filer. For filer med stor størrelse kan det være mer egnet å bruke programmeringsspråk som Python eller Java, hvor leseoperasjoner kan buffres og minnehåndtering håndteres mer effektivt.

## Se Også: 

For mer informasjon om tekstfil manupulering i Unix/Linux, sjekk ut følgende kilder:
- GNU `sed`: https://www.gnu.org/software/sed/manual/sed.html
- GNU `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- `cat`, `more` og `less`: https://www.gnu.org/software/coreutils/manual/html_node/index.html