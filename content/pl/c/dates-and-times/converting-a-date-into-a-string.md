---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:26.657873-07:00
description: "Jak to zrobi\u0107: Do tego celu cz\u0119sto u\u017Cywana jest funkcja\
  \ `strftime` z biblioteki `<time.h>`. Pozwala ona na formatowanie daty i czasu w\
  \ r\xF3\u017Cnorodny spos\xF3b,\u2026"
lastmod: '2024-03-13T22:44:35.898904-06:00'
model: gpt-4-0125-preview
summary: "Do tego celu cz\u0119sto u\u017Cywana jest funkcja `strftime` z biblioteki\
  \ `<time.h>`."
title: "Konwersja daty na ci\u0105g znak\xF3w"
weight: 28
---

## Jak to zrobić:
Do tego celu często używana jest funkcja `strftime` z biblioteki `<time.h>`. Pozwala ona na formatowanie daty i czasu w różnorodny sposób, poprzez określenie specyfikatorów formatu. Oto krótki przykład:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Konwersja daty i czasu na ciąg znaków (np. "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Aktualna data i czas: %s\n", dateStr);
    return 0;
}
```

Przykładowe wyjście może wyglądać tak:

```
Aktualna data i czas: Wed Jun 30 21:49:08 2021
```

Można dostosować format, zmieniając specyfikatory formatów przekazywane do `strftime`. Na przykład, aby uzyskać datę w formacie `RRRR-MM-DD`, użyłbyś `"%Y-%m-%d"`.

## Pogłębienie tematu
Funkcja `strftime` oraz biblioteka `<time.h>` są częścią Standardowej Biblioteki C, która sięga czasów oryginalnego standardu ANSI C (C89/C90). Choć ta metoda jest prosta i obsługiwana na wielu platformach, w porównaniu z nowoczesnymi językami programowania, które oferują bardziej intuicyjne biblioteki daty i czasu, może wydawać się niskopoziomowa i uciążliwa.

Należy zauważyć, że choć funkcje czasu standardowej biblioteki C są szeroko obsługiwane i stosunkowo proste w użyciu, brakuje im niektórych bardziej złożonych funkcji manipulacji strefą czasową i internacjonalizacji, które można znaleźć w bibliotekach nowszych języków lub w bibliotekach stron trzecich dla C, takich jak Międzynarodowe Komponenty dla Unicode (ICU).

Jednak możliwości dostosowywania funkcji `strftime` oraz szerokie wsparcie platformowe czynią ją niezawodnym i przydatnym narzędziem do konwersji daty na ciąg znaków w C. Programiści pochodzący z języków z bibliotekami daty i czasu na wyższym poziomie abstrakcji mogą potrzebować dostosować się do jej niskopoziomowego charakteru, ale znajdą ją wyjątkowo potężną i wszechstronną do formatowania dat i czasów dla różnorodnych zastosowań.
