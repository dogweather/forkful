---
title:    "C: Oppretting av en midlertidig fil"
keywords: ["C"]
---

{{< edit_this_page >}}

##Hvorfor

I denne blogginnlegget vil vi ta en titt på hvordan man oppretter midlertidige filer i C-programmering. Å lage midlertidige filer er ofte nyttig når man trenger å lagre data midlertidig under utførelsen av et program. Dette kan inkludere caching av data, midlertidig lagring av input eller lignende.

##Slik gjør du det

For å opprette en midlertidig fil, må du bruke C-funksjonen "tmpfile()" som er definert i "stdio.h" biblioteket. Denne funksjonen oppretter en midlertidig fil og returnerer en peker til den. La oss se på et eksempel:

```C
#include <stdio.h>

int main(){
  FILE *fp;
  fp = tmpfile(); // Oppretter en midlertidig fil og lagrer en peker til den i fp
  
  fprintf(fp, "Dette er en midlertidig fil\n"); // Skriver data til filen
  fflush(fp); // Sørger for at dataen faktisk blir lagret på disk
  fclose(fp); // Lukker filen
  
  return 0;
}
```
Output av dette programmet vil være en midlertidig fil som heter "fileXXXXXX" (med X-er som står for en unik identifikator). Denne filen vil inneholde teksten "Dette er en midlertidig fil". Merk at denne filen vil bli automatisk slettet når programmet avsluttes og filen lukkes.

##Dypdykk

Når man bruker "tmpfile()" funksjonen, vil C automatisk generere en unik filnavn og opprette filen for deg. Denne midlertidige filen vil bli lagret i det temporære systemet knyttet til ditt operativsystem. Du kan også opprette en midlertidig fil ved å bruke "tmpnam()" funksjonen, som lar deg definere filnavnet selv. 

En ting å huske på når man bruker midlertidige filer er at de kan risikere å bli slettet av systemet ditt hvis det er for lite plass på harddisken. Det er derfor viktig å sørge for at det er nok ledig plass når du oppretter midlertidige filer.

##Se også

- [tmpfile() documentation](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [tmpnam() documentation](https://www.tutorialspoint.com/c_standard_library/c_function_tmpnam.htm)
- [How to create a temporary file in C](https://www.geeksforgeeks.org/tmpfile-function-in-c-with-examples/)
- [Deleting a temporary file in C](https://stackoverflow.com/questions/7777823/how-to-remove-a-temp-file-in-c)