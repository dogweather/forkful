---
title:                "Lese en tekstfil"
html_title:           "C: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Lesing av en tekstfil er en vanlig oppgave for programmerere. Det betyr rett og slett at vi leser innholdet i en fil, som er en samling av tekst og data, inn i koden vår. Dette er nyttig når vi ønsker å hente ut informasjon fra en fil og bruke den i vårt program.

# Hvordan:
Vi kan lese en tekstfil ved hjelp av standard I/O-funksjoner i C. Vi trenger først å åpne filen vi ønsker å lese ved hjelp av `fopen()`-funksjonen. Deretter kan vi bruke `fgets()`-funksjonen til å lese hver linje i filen og lagre den i en variabel. Til slutt må vi huske å lukke filen ved hjelp av `fclose()`-funksjonen når vi er ferdige med å lese fra den.

``` C
FILE *fp;
char line[255];

fp = fopen("tekstfil.txt", "r");

while (fgets(line, 255, fp) != NULL) {
  printf("%s", line);
}

fclose(fp);
```

Dette eksempelet åpner en tekstfil med navnet "tekstfil.txt" for lesing og leser linjene en etter en til `fgets()` returnerer `NULL`, som betyr at det ikke er flere linjer igjen å lese. Deretter skrives hver linje ut til skjermen ved hjelp av `printf()`-funksjonen. Til slutt lukkes filen igjen.

# Dypdykk:
Lesing av tekstfiler har vært en viktig del av programmering siden de første programmene ble skrevet. Det finnes også flere alternative måter å lese tekstfiler på, som for eksempel å bruke `fscanf()`-funksjonen eller å bruke ferdiglagde biblioteker som `libfread`. Implementeringen av å lese en tekstfil avhenger også av hvilket operativsystem vi kjører på, da filsystemet er forskjellig fra system til system.

# Se Også:
- [C File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Reading and Writing Files in C](https://www.geeksforgeeks.org/basics-file-handling-c/)