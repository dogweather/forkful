---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Arduino: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Konwertowanie daty na ciąg znaków jest procesem, w którym przekształcamy datę w formacie tekstowym. Programiści często wykonują to w celu łatwego wyświetlenia lub przetwarzania dat w swoim kodzie.

## Jak to zrobić:

```Arduino
#include <TimeLib.h>

void setup() {
    int month = 2; // numerical representation of month
    int day = 14; // day of the month
    int year = 2020; // year
    char dateString[11]; // string to store the date
    sprintf(dateString, "%02d/%02d/%04d", month, day, year); // converting date to string with format "mm/dd/yyyy"
    Serial.print(dateString); // printing the string
}
 
void loop() {
    // other code here
}

```

Output: ```02/14/2020```

## W głębi duszy:

Konwersja daty na ciąg znaków jest ważna z kilku powodów. Po pierwsze, pozwala na łatwe wyświetlanie dat w różnych formatach na ekranie lub innym urządzeniu wyjściowym. Po drugie, może ułatwić wykonywanie obliczeń związanych z datami w kodzie, ponieważ daty przekonwertowane do postaci tekstowej są łatwiejsze do porównania i manipulacji. Inną metodą konwersji daty na ciąg znaków jest użycie biblioteki String, jednak jest to mniej wydajne i wymaga więcej zasobów pamięci.

## Zobacz też:

- [Biblioteka TimeLib do obsługi czasu i daty w Arduino](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Dokumentacja funkcji sprintf() w języku C](http://www.cplusplus.com/reference/cstdio/sprintf/)