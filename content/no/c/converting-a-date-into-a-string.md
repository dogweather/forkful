---
title:                "Konvertere en dato til en streng"
date:                  2024-01-20T17:35:52.395623-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng innebærer å omdanne datotypen som representerer tidspunkter til tekstformat. Dette gjøres for å lettere vise datoer for brukere eller å lagre dem i tekstbaserte formater som JSON eller CSV.

## Slik gjør du det:
For å konvertere en dato til en streng i C, kan du bruke `strftime`-funksjonen. Koden nedenfor viser et enkelt eksempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char datoStreng[100];
    time_t naa = time(NULL);
    struct tm *tidStruktur = localtime(&naa);

    strftime(datoStreng, sizeof(datoStreng), "%d-%m-%Y %H:%M", tidStruktur);
    printf("Dagens dato og tid: %s\n", datoStreng);

    return 0;
}
```

Forventet utdata:
```
Dagens dato og tid: 24-03-2023 14:55
```

## Dypdykk:
`strftime` funksjonen har vært en del av C standardbiblioteket siden C89-standarden og gir formateringsmuligheter for dato og tid som er fleksible. Alternativer til `strftime` kan være `sprintf` kombinert med individuelle tidkomponenter eller bruk av tredjeparts biblioteker som `date.h` for mer komplekse behov. Når du implementerer datokonvertering, bør du tenke på lokalitet, siden datoformatet kan variere fra sted til sted.

## Se også:
- C Standard Library: https://en.cppreference.com/w/c/chrono
- `date.h`: https://github.com/HowardHinnant/date
- JSON-formatet: https://www.json.org/json-en.html
- CSV-formatet: https://tools.ietf.org/html/rfc4180