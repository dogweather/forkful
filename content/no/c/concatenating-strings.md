---
title:    "C: Sammenstilling av strenger"
keywords: ["C"]
---

{{< edit_this_page >}}

C-programmering for nybegynnere

## Hvorfor

Å kombinere to tekststrenger er en viktig del av C-programmering. Dette gjør det mulig å lage mer komplekse tekststrenger som kan brukes til å presentere informasjon til brukeren. Dette er spesielt nyttig når du jobber med input-output-operasjoner eller når du trenger å generere dynamiske tekster.

## Hvordan

Det første du må gjøre er å definere to tekstvariable som du ønsker å kombinere. Dette kan gjøres ved å bruke datatype "char". Deretter kan du bruke "strcat" funksjonen til å kombinere de to stringene. For eksempel:

```C
char navn1[20] = "John";
char navn2[20] = "Doe";
strcat(navn1, navn2);
printf("%s", navn1);
```

Dette vil kombinere "John" og "Doe" til en string og skrive ut "JohnDoe" til skjermen. Det er også viktig å merke seg at "strcat" funksjonen endrer verdien til den første stringen, så det er viktig å sørge for at den første stringen har nok plass til å inkludere den andre stringen.

Du kan også kombinere flere stringer ved å bruke flere "strcat" funksjoner. For eksempel:

```C
char adjektiv[20] = "stilig";
strcat(adjektiv, navn1);
strcat(adjektiv, "!"); 
printf("Du ser veldig %s ut, %s", adjektiv, navn2);
```

Dette vil kombinere "stilig", "John" og "!" og skrive ut "Du ser veldig stilig ut, John!" til skjermen.

## Dypdykk

I tillegg til "strcat" funksjonen, er det også en rekke andre funksjoner som kan brukes til å kombinere tekststrenger, for eksempel "strcpy" og "sprintf". Disse funksjonene har forskjellige bruksområder og kan være nyttige i ulike situasjoner.

Det er også viktig å være oppmerksom på at ASCII-karakteren for null, "\0", er avgjørende for å kombinere stringer riktig. Dette er fordi C bruker denne karakteren for å markere slutten på en string. Derfor må du sørge for at den første stringen har en null-karakter på slutten før du bruker "strcat" funksjonen.

## Se Også

- [https://www.programiz.com/c-programming/library-function/string.h/strcat](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [https://www.geeksforgeeks.org/strcat-in-ccpp/](https://www.geeksforgeeks.org/strcat-in-ccpp/)
- [https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)