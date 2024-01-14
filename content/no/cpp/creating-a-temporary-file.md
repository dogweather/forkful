---
title:                "C++: Å opprette en midlertidig fil"
simple_title:         "Å opprette en midlertidig fil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lage midlertidige filer kan være nyttig i programmering for å lagre og midlertidig behandle data. Dette kan være spesielt nyttig når man jobber med store datamengder eller trenger å sikkerhetskopiere data.

## Hvordan

For å lage en midlertidig fil i C++, kan man bruke funksjonen `tmpfile()` fra `<stdio.h>` biblioteket. Her er et eksempel på hvordan man kan lage og skrive til en midlertidig fil:

```C++
#include <stdio.h>
int main() {
    FILE *fp;
    int num = 5;
    fp = tmpfile();
    fprintf(fp, "%d", num);
    fclose(fp);
    return 0;
}
```

Dette eksemplet vil lage en midlertidig fil og skrive verdien "5" til den. Man kan også bruke `fwrite()` for å skrive flere verdier til filen. Det er viktig å huske å lukke filen etter man er ferdig med å skrive til den.

## Dypdykk

Når man lager en midlertidig fil, blir den automatisk slettet når programmet avsluttes. Det kan også være nyttig å vite at man kan spesifisere hvor filen skal opprettes ved å bruke funksjonen `tmpnam()` fra samme bibliotek.

For å få tilgang til filen senere, kan man bruke funksjonen `tmpfile()` igjen og få en "filpeker" tilbake. Man kan også bruke `fread()` for å lese fra filen.

## Se også

- C++ `tmpfile()` reference: [https://www.cplusplus.com/reference/cstdio/tmpfile/](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- C++ `fwrite()` reference: [https://www.cplusplus.com/reference/cstdio/fwrite/](https://www.cplusplus.com/reference/cstdio/fwrite/)
- C++ `fread()` reference: [https://www.cplusplus.com/reference/cstdio/fread/](https://www.cplusplus.com/reference/cstdio/fread/)