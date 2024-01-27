---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Standardfeil (`stderr`) lar deg skrive feilmeldinger og diagnostikk uavhengig av vanlig utdata (`stdout`). Programmerere bruker det for å rapportere feil uten å forstyrre programmets vanlige utdatastrøm.

## Hvordan gjøre det:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Feil oppstått: Ugyldig brukerinndata.\n");
    return 0;
}
```
Sample output:
```
Feil oppstått: Ugyldig brukerinndata.
```

## Dyptdykk
I Unix-lignende systemer er `stderr` filstrøm nummer to (med `stdin` som 0 og `stdout` som 1). I motsetning til `stdout`, er `stderr` ikke bufret, noe som betyr at feilmeldinger vises umiddelbart. Alternativer til standardfeil inkluderer logging til en fil eller bruk av dedikerte logging-rammeverk. Implementasjonsdetaljer er plattformspesifikke, men konvensjonen for standardfeil er konsekvent over de fleste systemer.

## Se Også
- GNU C Library dokumentasjon på Standard Streams: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- C Standard Library `<stdio.h>`: https://en.cppreference.com/w/c/io
- Lær om Unix filstrømmenes filbeskrivelsesnummer: https://en.wikipedia.org/wiki/File_descriptor
