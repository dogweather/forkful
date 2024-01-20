---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil er en metode for å lagre data midlertidig under programutførelse. Progammerere gjør dette for å behandle store data, dele data mellom prosesser, eller bevare tilstanden til data hvis programmet avbrytes.

## Hvordan:
Her er et grunnleggende eksempel på C koden for å lage en midlertidig fil:

```C
#include<stdio.h>

int main() {
   char tmpname[L_tmpnam];
   char* filename = tmpnam(tmpname);

   printf("Temporary file name is: %s\n", filename);
   return 0;
}
```
Når koden kjører, vil output se slik ut:
```
Temporary file name is: /tmp/a1b2c3
```
Dette er navnet på den midlertidige filen som programmene dine kan bruke til å lagre data.

## Dypdykk
Oppretting av midlertidige filer har en lang historie i programmering. De ble først brukt da minne og lagringsplass var dyrebare ressurser og har fortsatt å være nyttige for mange moderne brukstilfeller.

Alternativer inkluderer bruk av midlertidige tabeller i databaser, eller minnebasert lagring, som RAM-disker. Valget avhenger av dine spesifikke behov og begrensninger.

Når du oppretter en midlertidig fil i C, bruker du funksjonen `tmpnam()`. Denne funksjonen genererer et unikt filnavn som kan brukes til å opprette filen. Filen er ikke faktisk opprettet når du ringer `tmpnam()`. Du må opprette og åpne den med funksjonen `fopen()` hvis du faktisk vil lagre data i den.

## Se også
- [tmpfile function in C](https://www.cplusplus.com/reference/cstdio/tmpfile/) 
- [The C library function tmpnam()](https://www.tutorialspoint.com/c_standard_library/c_function_tmpnam.htm)