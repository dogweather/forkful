---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja daty na ciąg (ang. string) to proces zamiany daty na ciąg znaków. Programiści przeprowadzają tę operację aby mogli łatwo porównać, wyświetlić lub zapisywać daty w dowolnym formacie.

## Jak to zrobić:

W Arduino, możesz użyć funkcji `sprintf()`, aby przekształcić daty w ciągi znaków.

```Arduino
char dataString[10];
int dzien = 30;
int miesiac = 12;
int rok = 2021;

sprintf(dataString, "%02d-%02d-%d", dzien, miesiac, rok);

Serial.begin(9600);
Serial.println(dataString);
```

To wykonane w Arduino zwraca na wyjściu:

```Arduino
30-12-2021
```

## Głębsze Zanurzenie

Konwersja daty na ciąg znaków jest praktyką obserwowaną od początków komputeryzacji. Ułatwia to porównywanie dat i ich zapisywanie w systemach baz danych. Alternatywą dla `sprintf()` w Arduino mogą być różne biblioteki do manipulacji ciągami znaków i dat.

Sposób implementacji zależy od konkretnych wymagań projektu. Przykładowo, niektórzy mogą preferować formatowanie daty na "DD-MM-YYYY", inni "MM/DD/YYYY" lub nawet "YYYYMMDD". 

## Zobacz także

- Dokumentacja dla sprintf(): http://www.cplusplus.com/reference/cstdio/sprintf/
- Kurs Arduino: https://arduino.pl/kurs
- Biblioteka TimeLib na GitHub: https://github.com/PaulStoffregen/Time
- Dokumentacja Arduino po polsku: https://arduino.org.pl/dokumentacja/