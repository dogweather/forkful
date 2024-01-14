---
title:                "C: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Hvorfor

Mange ganger i C-programmering, kan vi trenge å opprette en midlertidig fil i løpet av kjøretiden. Dette kan være for å lagre midlertidige data, mellomresultater eller som en sikkerhetskopieringsmetode. Å vite hvordan du oppretter en midlertidig fil kan være nyttig, spesielt for å skrive robuste og effektive programmer.

##Slik gjør du det

For å opprette en midlertidig fil i C, bruker vi "tmpfile()" funksjonen. Denne funksjonen åpner en midlertidig fil for lesing og skriving, og returnerer en peker til fila. Vi kan deretter bruke denne pekeren til å skrive og lese fra den midlertidige filen.

```C
#include <stdio.h>

int main()
{
  FILE *tmpfile_ptr; // deklarere en peker til den midlertidige filen
  char buffer[100] = "Dette er en midlertidig fil";

  tmpfile_ptr = tmpfile(); // opprett den midlertidige filen

  if (tmpfile_ptr != NULL) // sjekk om filen ble opprettet riktig
  {
    fwrite(buffer, sizeof(char), strlen(buffer), tmpfile_ptr); // skriv til den midlertidige filen
    fseek(tmpfile_ptr, 0, SEEK_SET); // plasser pekeren i starten av filen
    fread(buffer, sizeof(char), strlen(buffer), tmpfile_ptr); // les fra den midlertidige filen

    printf("%s\n", buffer); // skriv ut innholdet i filen
    fclose(tmpfile_ptr); // lukk den midlertidige filen
  }

  return 0;
}
```

Forventet utdata:

```
Dette er en midlertidig fil
```

##Dypdykk

Når vi oppretter en midlertidig fil ved hjelp av "tmpfile()", opprettes det automatisk en unik fil med et unikt navn på et midlertidig sted på systemet vårt. Dette betyr at vi ikke trenger å bekymre oss for å gi filen et passende navn eller å slette den etter bruk. Dette gjør prosessen med å arbeide med midlertidige filer enklere og mer effektiv.

En annen viktig ting å merke seg er at den midlertidige fila bare er tilgjengelig mens programmet vårt kjører. Når du avslutter programmet, blir filen automatisk slettet fra systemet. Dette er nyttig for å unngå å forlate unødvendige filer på datamaskinen vår.

##Se også

- [C Programming Tutorial: Creating and Writing to a File](https://www.learn-c.org/en/File_IO)
- [C Standard Library: tmpfile() function](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Creating and Managing Temporary Files with C](https://www.howtogeek.com/50364/using-the-tmpfile-function-in-c-for-easy-file-management/)