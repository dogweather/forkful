---
title:                "C: Łączenie ciągów tekstowych."
simple_title:         "Łączenie ciągów tekstowych."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele sytuacji, w których konieczne jest połączenie dwóch lub więcej ciągów znaków w jedną dłuższą linię. Na przykład, gdy chcemy wyświetlić imię i nazwisko użytkownika oraz datę urodzenia, musimy połączyć trzy oddzielne ciągi znaków w jedną linię. W takich przypadkach, wykorzystanie funkcji do konkatencji znaków jest niezbędne, ponieważ ułatwia to manipulowanie tekstem i tworzenie bardziej czytelnych wyjść.

## Jak To Zrobić?

W języku C, możemy skorzystać z funkcji ```sprintf()```, aby połączyć dwa lub więcej ciągów znaków i zapisać wynik w określonej zmiennej. Przykładowy kod wyglądałby następująco:

```
char imie[20] = "Anna";
char nazwisko[20] = "Kowalska";
char data_urodzenia[10] = "01.01.1990";

char wynik[50];
sprintf(wynik, "%s %s urodzona/born/a %s", imie, nazwisko, data_urodzenia);
printf("%s", wynik);
```

Powinniśmy używać ```sprintf()``` z ostrożnością, ponieważ nie ma ograniczenia długości wynikowego ciągu znaków, co może powodować wyjście poza zaalokowaną pamięć. Możemy również skorzystać z funkcji ```snprintf()```, która pozwala określić maksymalną długość wynikowego ciągu.

## Głębszy Zanurzenie

W przypadku bardziej złożonych operacji związanych z konkatencją znaków, istnieją również inne funkcje w języku C, takie jak ```strcat()```, ```strncat()``` czy ```strtok()```, które pozwalają na dodatkowe funkcjonalności, takie jak łączenie na podstawie określonego delimitera czy łączenie tylko części znaków.

Jednakże, należy uważać na wykorzystywanie tych funkcji, ponieważ mogą być podatne na błędy związane z przepełnieniem bufora.

## Zobacz również

- [Funkcje do manipulacji łańcuchami w języku C](https://www.programiz.com/c-programming/string-handling-functions)
- [Przykłady użycia funkcji konkatencji znaków w języku C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C string functions - C przewodnik dla programistów](https://www.programiz.com/c-programming/string-functions)