---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zamiana daty z ciągu znaków, czyli 'parsing', to proces, w którym data reprezentowana jako tekst zostaje przekształcona w format daty. Programiści robią to, aby móc manewrować i manipulować datami schowanymi w tekście.

## Jak to zrobić:
Przykład kawałka kodu pokazującego jak to zrobić. Załóżmy, że nasza data jest zapisana w formacie "DD-MM-YYYY".

```Arduino
#include <TimeLib.h> 
String dateString = "31-12-2020";
int day = dateString.substring(0,2).toInt();
int month = dateString.substring(3,5).toInt();
int year = dateString.substring(6,10).toInt();
```
Teraz mamy dzień, miesiąc i rok zapisane jako liczby, które możemy łatwo manipulować.

```Arduino
tmElements_t tm;

tm.Day = day;
tm.Month = month;
tm.Year = CalendarYrToTm(year);

time_t t = makeTime(tm);
```
Z powyższego kodu wynika, że nasza data jest teraz pakowana do struktury `tmElements_t`.

## Głębszy Wgląd
"Parsing" daty z tekstu jeszcze nie tak dawno było trudnym zdaniem. Wcześniej programiści musieli samodzielnie pisać wszystko, od wymiany danych między różnymi formatami do obsługi wszelkiego rodzaju wyjątków, takich jak lata przestępne.

Istnieją alternatywne metody parsowania daty, ale używając biblioteki TimeLib, proces ten jest znacznie uproszczony. Nie mniej jednak, zawsze warto zrozumieć podstawowe kwestie, które stoją za takimi działaniami, takie jak różnice między systemami czasu.

Szczegół implementacji to wykorzystanie biblioteki TimeLib do łatwego manipulowania danymi typu czasowo-datowego. Wszystko, co musisz zrobić, to sparsować dzień, miesiąc i rok, umieścić je w odpowiedziach lub użyć, jak chcesz.

## Zobacz również:
1. [Dokumentacja Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
2. [Jak parsować datę z ciągu - Stack Overflow](https://stackoverflow.com/questions/5590381/easiest-way-to-convert-int-to-string-in-c)