---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana daty na ciąg znaków w C to proces konwertowania danych typu "Data" do formatu String. Programiści robią to, aby łatwiej obsługiwać i prezentować dane związane z datą.

## Jak to zrobić:

Śledź poniższy kod, który demonstruje, jak przekształcić datę w ciąg znaków w C.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;

    time (&rawtime);
    timeinfo = localtime (&rawtime);

    printf ("Obecna data i czas to: %s", asctime(timeinfo));

    return 0;
}
```

Wyjście z tego kodu może prezentować się następująco:

```C
Obecna data i czas to: Sun Sep 26 21:24:06 2021
```

## Głębsze zrozumienie:

(1) Kontekst historyczny: W pierwotnej wersji C, nie było bezpośredniej funkcji do konwersji daty na string. Programiści musieli samodzielnie tworzyć takie funkcje.

(2) Alternatywy: Możemy korzystać również z funkcji `strftime()`, która pozwala na konwersję daty do ciągu znaków z niestandardowym formatowaniem.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm* timeinfo;
    char buffer[80];

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    strftime(buffer,sizeof(buffer),"%d-%m-%Y %H:%M:%S",timeinfo);
    printf("Obecna data i czas to: %s", buffer);

    return 0;
}
```

(3) Detyale implementacji: `time()` zwraca aktualny czas, `localtime()` konwertuje sekundy na strukturę `tm`, a `asctime()` / `strftime()` przekształca strukturę `tm` do odpowiedniego formatu string.

## Zobacz też:

- Opis bazy ANSI C: 
  [http://www.open-std.org/jtc1/sc22/wg14/](http://www.open-std.org/jtc1/sc22/wg14/)
  
- Więcej na temat funkcji czasu w C: 
  [https://www.tutorialspoint.com/c_standard_library/time_h.htm](https://www.tutorialspoint.com/c_standard_library/time_h.htm) 

- Więcej informacji na temat asctime:
  [https://www.cplusplus.com/reference/ctime/asctime/](https://www.cplusplus.com/reference/ctime/asctime/) 

- Więcej na temat strftime: 
  [https://www.cplusplus.com/reference/ctime/strftime/](https://www.cplusplus.com/reference/ctime/strftime/)