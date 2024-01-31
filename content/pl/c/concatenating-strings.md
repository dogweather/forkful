---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:34:24.898414-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Łączenie łańcuchów znaków, czyli konkatenacja, to po prostu sklejanie razem dwóch czy więcej ciągów tekstowych. Programiści robią to, aby tworzyć dynamiczne komunikaty, ścieżki plików, zapytania SQL, i więcej – wszędzie tam, gdzie teksty muszą się zmieniać i dostosowywać on-the-fly.

## How to: (Jak to zrobić?)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Cześć, ";
    char str2[] = "świat!";

    // Konkatenacja za pomocą strcat()
    strcat(str1, str2);

    printf("%s\n", str1); // Wydruk: Cześć, świat!
    return 0;
}
```
Pamiętaj, by sprawdzić, czy bufor docelowy jest wystarczająco duży, aby pomieścić oba łańcuchy znaków razem z kończącym zerem.

## Deep Dive (Głębsze spojrzenie)
Po pierwsze, łańcuchy znaków w C nie są obiektami czy klasami jak w niektórych nowszych językach; to zwykłe tablice znaków zakończone zerem (`'\0'`). Historycznie, funkcje takie jak `strcat()` z biblioteki standardowej C były głównymi narzędziami do konkatenacji.

Alternatywy? Możesz użyć `sprintf()` dla skomplikowanych operacji łączenia, które też formatują tekst, albo skopiować łańcuchy znaków samodzielnie za pomocą pętli.

Kwestie implementacyjne: `strcat()` może prowadzić do przepełnienia bufora, jeżeli nie będziesz ostrożny. Nowoczesne podejście to używanie `strncat()`, gdzie określasz maksymalną ilość znaków do dodania – bezpieczniej.

## See Also (Zobacz również)
- [Biblioteka standardowa C](http://www.cplusplus.com/reference/cstring/)
- [Bezpieczne wersje funkcji](https://en.cppreference.com/w/c/string/byte/strncat)
- [Dokumentacja języka C](http://en.cppreference.com/w/c)
