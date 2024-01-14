---
title:    "C: Søke og erstatte tekst"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har jobbet med tekstbehandling eller skrevet lange tekster i notisblokker, har du sannsynligvis støtt på utfordringen med å endre store deler av teksten på en gang. Dette kan være en tidkrevende og kjedelig prosess, spesielt hvis teksten er lang og kompleks. Her kommer søk og erstatt-funksjonen i C-programmering inn i bildet. Ved å bruke denne funksjonen kan du effektivt søke gjennom teksten og erstatte spesifikke deler av den på en rask og enkel måte.

## Hvordan

For å søke og erstatte tekst i C-programmering må du bruke funksjonen `str_replace ()`. Denne funksjonen tar imot fire parametere. Den første parameteren er en tekststreng som skal søkes etter, den andre er en tekststreng som skal erstatte den første teksten, den tredje er den totale lengden på teksten som skal søkes og den fjerde er teksten der søket skal utføres. La oss se på et eksempel for å forstå bedre:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
    char tekst[] = "Dette er en testtekst";
    char old[] = "test";
    char new[] = "kode";
    int lengde = strlen(tekst);

    printf("%s", tekst);

    str_replace(old, new, lengde, tekst);
    printf("%s", tekst);

    return 0;
}

void str_replace(char old[], char new[], int lengde, char tekst[])
{
    int i, j;
    int n = strlen(new);
    int m = strlen(old);
    
    for(i = 0; i < lengde; i++)
    {
        if(strncmp(&tekst[i], old, m) == 0)
        {
            for(j = 0; j < n; ++j)
            {
                tekst[i + j] = new[j];
            }
            
            i += n - 1;
        }
    }
}
```

I dette eksempelet har vi en tekststreng "Dette er en testtekst" og vi vil endre "test" til "kode". Derfor kalles `str_replace ()` med disse verdiene. Outputen vil være "Dette er en kode tekst".

## Dypdykk

I eksemplet ovenfor erstatter vi bare én forekomst av ordet "test". Men hvis vi ønsker å erstatte alle forekomster av et ord i en tekst, må vi bruke en løkke for å gjennomgå hele teksten og erstatte alle tilsvarende ord. `Str_replace()`-funksjonen fungerer ved å sammenligne teksten med den gamle verdien og bytte ut den gamle verdien med den nye. Det er også mulig å følge den samme prosessen for å erstatte deler av en tekst ved å endre parametrene og funksjonsdefinisjonen.

## Se Også

- [Tutorial: Søke og erstatte tekst i C](https://www.includehelp.com/c-programs/search-and-replace-a-word-in-a-string-in-c-programming.aspx)
- [Veiledning: Søke og erstatte deler av tekst i C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [GitHub: Kildekode for `str_replace` funksjonen i C](https://github.com/mark-culhane/C/blob/master/str_replace.c)