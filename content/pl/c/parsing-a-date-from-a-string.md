---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza daty z ciągu (stringu) to proces ekstrakcji konkretnych informacji o dacie z tekstu. Programiści to robią, aby przekształcić dane wejściowe w postaci tekstowej na format, który jest łatwiejszy do przetworzenia i analizy.

## Jak to zrobić:

Przykład programu, który analizuje datę ze stringa:

```C
#include <stdio.h>
#include <time.h>

int main(){
    struct tm tm;
    char buf[255];

    memset(&tm, 0, sizeof(struct tm));
    strptime("2022-09-26", "%Y-%m-%d", &tm);
    strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);

    printf("Data: %s\n", buf);    

    return 0;
}
```

Gdy uruchomisz ten kod, otrzymasz następującą odpowiedź:

```C
Data: poniedziałek, wrzesień 26, 2022
```

## Dogłębne zanurzenie

1. **Kontekst historyczny**: Funkcję `strptime` dodano do biblioteki C w 1989 roku, a początkowo była dostępna tylko na platformach BSD i Linux. Obecnie jest dostępna w większości dystrybucji C.

2. **Alternatywy**: Chociaż najczęściej używanymi funkcjami są `strptime` i `strftime`, inne biblioteki, takie jak `getdate`, także mogą być używane do rozwiązania tego problemu.

3. **Szczegóły implementacji**: `strptime` musi być zdefiniowane przez bibliotekę czasu. Ponieważ może być to zdefiniowane różnie na różnych systemach, zachowanie może się różnić.

## Zobacz również

1. Dokumentacja na temat funkcji `strptime`: https://man7.org/linux/man-pages/man3/strptime.3.html

2. Więcej informacji na temat zrozumienia formatów daty i czasu: https://strftime.org/

3. Dyskusja na StackOverflow na temat analizy daty z ciągu: https://stackoverflow.com/questions/2891494/how-do-i-use-strptime