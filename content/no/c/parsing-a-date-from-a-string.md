---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse en dato fra en streng er prosessen med å konvertere en tekstrepresentasjon av en dato til en brukbar datatyp i programmering. Vi programmerer gjør det fordi det gir oss muligheten til å manipulere og bruke disse datoene på en mer effektiv måte i vår kode.

## Hvordan:

Angående eksempel, vi kan bruke `strptime` funksjonen i C for å parse en dato fra en streng. Her er et eksempel på hvordan du kan gjøre det.

```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm tm;
    char buf[255];

    strptime("2022-05-12", "%Y-%m-%d", &tm);
    strftime(buf, sizeof(buf), "%Y-%m-%d", &tm);
    puts(buf);

    return 0;
}
```

I dette eksempelet vil output være `2022-05-12`.

## Dypdykk

Historisk sett, tidligere versjoner av C leverte ikke innebygde funksjoner for å parse datoer fra strenger, men dagens versjoner gir denne funksjonaliteten gjennom `time.h` biblioteket. Som alternativer kan "third-party" biblioteker som `boost.date_time` i C++ brukes. Ved implementering av datoparsing fra en streng, er det viktig å merke seg at forskjellige land har forskjellige formater for datoer. Du må derfor håndtere disse formatene ettersom `strptime` funksjonen tar hensyn til dette.

## Se Også

[Tutorial for parsing dates in C](https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm)

[C Date & Time library documentation](https://www.cplusplus.com/reference/ctime/)