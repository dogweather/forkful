---
title:    "Bash: Leser en tekstfil"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av tekstfiler er en viktig ferdighet for alle som jobber med programmering. Enten du jobber med store datasett eller bare trenger å få tak i spesifikke data fra en fil, så er evnen til å lese tekstfiler en grunnleggende kompetanse i Bash-programmering.

## Slik gjør du det

Her skal jeg vise deg hvordan du kan lese en tekstfil ved hjelp av Bash-skripting. Først må du ha en tekstfil tilgjengelig som du ønsker å lese. La oss si at filen vi skal lese heter "eksempel.txt". For å lese denne filen, må vi bruke kommandoen `cat` etterfulgt av filnavnet:

```Bash
cat eksempel.txt
```
Dette vil skrive ut det som står i filen, enten det er tekst eller tall. Hvis du ønsker å lagre innholdet i filen i en variabel, kan du bruke følgende kommando:

```Bash
INNHOLD=$(cat eksempel.txt)
```
Nå vil variabelen "INNHOLD" inneholde alt som står i filen. Hvis du bare ønsker å lese en spesifikk linje i filen, kan du bruke denne kommandoen:

```Bash
LINJE=$(head -n 1 eksempel.txt)
```
Dette vil lagre den første linjen i filen i variabelen "LINJE". Du kan justere hvor mange linjer du ønsker å lese ved å endre tallet etter "-n".

## Dypdykk

Når du leser en tekstfil med Bash, vil innholdet bli lest som en enkelt streng. Dette betyr at hvis filen inneholder flere linjer, så vil de bli behandlet som én lang linje. Du kan også bruke "read" kommandoen til å lese en fil linje for linje. For eksempel:

```Bash
while read linje; do
    echo $linje
done < eksempel.txt
```
Dette vil lese filen linje for linje og skrive ut hver linje i terminalen.

## Se også

* [Bash Command Line Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
* [Reading and Writing Files in Bash](https://www.linuxjournal.com/content/bash-reading-and-writing-files)