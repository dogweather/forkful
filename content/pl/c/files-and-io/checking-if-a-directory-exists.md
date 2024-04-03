---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:48.671090-07:00
description: "Jak to zrobi\u0107: W C, istnienie katalogu mo\u017Cna sprawdzi\u0107\
  \ za pomoc\u0105 funkcji `stat`, kt\xF3ra pobiera informacje o pliku lub katalogu\
  \ znajduj\u0105cym si\u0119 pod\u2026"
lastmod: '2024-03-13T22:44:35.902193-06:00'
model: gpt-4-0125-preview
summary: "W C, istnienie katalogu mo\u017Cna sprawdzi\u0107 za pomoc\u0105 funkcji\
  \ `stat`, kt\xF3ra pobiera informacje o pliku lub katalogu znajduj\u0105cym si\u0119\
  \ pod okre\u015Blon\u0105 \u015Bcie\u017Ck\u0105."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
W C, istnienie katalogu można sprawdzić za pomocą funkcji `stat`, która pobiera informacje o pliku lub katalogu znajdującym się pod określoną ścieżką. Następnie używa się makra `S_ISDIR` z `sys/stat.h` do oceny, czy uzyskane informacje odpowiadają katalogowi.

Oto jak można użyć `stat` i `S_ISDIR`, aby sprawdzić, czy katalog istnieje:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Ścieżka katalogu do sprawdzenia
    char *dirPath = "/sciezka/do/katalogu";

    // Pobierz status ścieżki
    int result = stat(dirPath, &stats);

    // Sprawdź, czy katalog istnieje
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("Katalog istnieje.\n");
    } else {
        printf("Katalog nie istnieje.\n");
    }

    return 0;
}
```

Przykładowe wyjście:
```
Katalog istnieje.
```

Lub, jeśli katalog nie istnieje:
```
Katalog nie istnieje.
```

## Wgłębienie się:
Struktura i funkcja `stat` są częścią języka programowania C od dziesięcioleci, pochodząc z Unix. Zapewniają one ustandaryzowany sposób na pobieranie informacji o systemie plików, który, mimo że jest stosunkowo niskopoziomowy, jest szeroko używany ze względu na swoją prostotę i bezpośredni dostęp do metadanych systemu plików.

Historycznie rzecz biorąc, sprawdzanie istnienia i właściwości plików oraz katalogów za pomocą `stat` i jego pochodnych (takich jak `fstat` i `lstat`) było powszechnym podejściem. Jednak te funkcje bezpośrednio wchodzą w interakcje z jądrem OS, co może wprowadzać dodatkowe obciążenie i potencjalne błędy, jeśli nie są prawidłowo obsługiwane.

W przypadku nowych projektów lub przy pracy w scenariuszach wysokopoziomowych programiści mogą optować za bardziej abstrakcyjnymi mechanizmami obsługi plików dostarczanymi przez nowoczesne frameworki lub biblioteki, które obsługują błędy w bardziej zrównoważony sposób i zapewniają prostsze API. Jednak zrozumienie i umiejętność używania `stat` pozostają cenną umiejętnością w scenariuszach wymagających bezpośredniej manipulacji systemem plików, takich jak programowanie systemowe, lub gdy praca w ograniczonych środowiskach, gdzie zależność od dużych bibliotek jest niewykonalna.
