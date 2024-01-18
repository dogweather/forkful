---
title:                "Analiza daty ze łańcucha znaków."
html_title:           "C: Analiza daty ze łańcucha znaków."
simple_title:         "Analiza daty ze łańcucha znaków."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# O co chodzi i dlaczego?

Parsowanie daty z ciągu znaków to proces wyodrębniania informacji o dacie z tekstu w ustalonym formacie. Programiści często zajmują się tym, gdyż muszą przetwarzać dane wejściowe, które zawierają daty w postaci tekstu.

# Jak to zrobić?

Poniżej znajdują się przykładowe kody, które pokazują jak parsować datę z ciągu znaków w języku C. Pamiętaj, że dla większej czytelności kodu, umieszczamy tylko niezbędne elementy, takie jak wywołania funkcji i zmienne. Pełne przykłady możesz znaleźć w powiązanych źródłach.

```
#include <stdio.h>
#include <time.h>

int main()
{
    char date_str[] = "01/01/2020"; // Przykładowy ciąg znaków z datą
    struct tm date; // Struktura przechowująca informacje o dacie

    // Przetwarzamy tekst na datę używając funkcji strptime()
    if (strptime(date_str, "%d/%m/%Y", &date) != NULL)
    {
        // Teraz możemy wyświetlić poszczególne elementy daty
        printf("Dzień: %d\n", date.tm_mday);
        printf("Miesiąc: %d\n", date.tm_mon + 1); // Funkcja przetwarza miesiące od 0, więc musimy dodać 1
        printf("Rok: %d\n", date.tm_year + 1900); // Funkcja przetwarza lata od 1900
    }
    else
    {
        printf("Nieprawidłowy format daty\n");
    }

    return 0;
}
```

Wynikiem powyższego kodu powinno być:

```
Dzień: 1
Miesiąc: 1
Rok: 2020
```

# Głębszy zanurzenie

Parsowanie daty z ciągu znaków jest ważnym i powszechnym zadaniem w programowaniu. W przeszłości programiści musieli samodzielnie implementować funkcje do tego celu, jednak teraz język C oferuje wbudowane funkcje, takie jak `strptime()`, które ułatwiają ten proces.

Alternatywnym sposobem na parsowanie daty jest wykorzystanie biblioteki `libdatetime` lub `libcalender`, które oferują bardziej zaawansowane funkcje do przetwarzania dat.

Implementacja `strptime()` wykorzystuje tablicę formatu, która określa, w jakiej kolejności poszczególne elementy daty występują w tekście. Dzięki temu funkcja jest w stanie poprawnie przetworzyć datę bez względu na jej format.

# Zobacz także

- Dokumentacja `strptime()` w języku C: https://www.cplusplus.com/reference/ctime/strptime/?kw=strptime
- Informacje o bibliotece `libdatetime`: https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.1.0/com.ibm.zos.v2r1.bpxbd00/dstatim.htm
- Dokumentacja `libcalender`: https://man7.org/linux/man-pages/man3/calender.3.html