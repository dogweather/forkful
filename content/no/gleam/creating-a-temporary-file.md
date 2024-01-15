---
title:                "Opprette en midlertidig fil"
html_title:           "Gleam: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer i programmering kan være nyttig for å lagre data midlertidig eller for å lage en midlertidig kopi av en fil som du trenger å manipulere. Dette kan gjøres i Gleam ved å bruke standardbiblioteket "standard/tempfile".

## Slik gjør du det

For å opprette en midlertidig fil i Gleam, bruker du funksjonen "create" fra "tempfile" -biblioteket. Du kan velge å angi en prefiks og/eller suffiks for filnavnet, eller la Gleam velge en tilfeldig generert. For eksempel:

```Gleam
import standard/tempfile

let filnavn = tempfile.create(prefiks="midlertidig_", suffiks=".txt")
```
Dette vil resultere i opprettelsen av en tekstfil i temp-mappen med navnet "midlertidig_9xgl3jua.txt".

Du kan også legge til innhold i den midlertidige filen ved å bruke "write_all" -funksjonen og angir filnavnet og innholdet du vil skrive som argumenter. For eksempel:

```Gleam
tempfile.write_all(filnavn, "Dette er en midlertidig fil.")
```
Hvis du vil lese innholdet i den midlertidige filen, kan du bruke "read_all" -funksjonen og angi filnavnet som argument. For eksempel:

```Gleam
let innhold = tempfile.read_all(filnavn)
```
Dette vil resultere i at innholdet "Dette er en midlertidig fil." blir lagret i variabelen "innhold".

Merk: Det er viktig å huske å slette den midlertidige filen når den ikke lenger trengs. Dette kan gjøres ved å bruke "remove" -funksjonen og angi filnavnet som argument. For eksempel:

```Gleam
tempfile.remove(filnavn)
```

## Dypdykk

Bak kulissene bruker "tempfile" -biblioteket funksjonene "mkstemp" og "mkdtemp" fra standardbiblioteket "os" for å opprette midlertidige filer og mapper. Disse funksjonene genererer enten et tilfeldig navn eller bruker et prefiks og/eller suffiks for filnavnet. Det er også mulig å angi en bestemt mappe for å opprette den midlertidige filen / mappen i.

## Se også

https://gleam.run/libraries/standard#tempfile
https://gleam.run/_/os.html#mkstemp
https://gleam.run/_/os.html#mkdtemp