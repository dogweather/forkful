---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Ekstrakcja podciągów polega na wyodrębnianiu mniejszych ciągów znaków z większych. Programiści robią to, żeby uzyskać ważne elementy informacji z większej ilości danych.

## Jak to zrobić:
W Arduino możemy korzystać z funkcji `substring()` do wyodrębniania podciągów. Poniżej jest przykład użycia.

```Arduino
String tekst = "Witaj, Świecie!";
String podciag = tekst.substring(7, 14); // Wyodrębnia podciąg od indeksu 7 do 14
Serial.println(podciag); // Wypisze "Świecie!"
```

## Deep dive:
Funkcja `substring()` została wprowadzona w Arduino w celu lepszej manipulacji ciągami znaków. Co więcej, to jest rodzaj operacji, który jest bardzo powszechny w innych językach programowania. 

Alternatywą dla tej funkcji mogą być operacje na tablicach char - ale są one bardziej skomplikowane i mniej czytelne.

Co do szczegółów implementacji, to `substring()` zwraca nowy obiekt String, które jest podciągiem oryginalnego Stringa. Należy pamiętać, że indeksy są liczony od zera, więc pierwszy znak w ciągu ma indeks 0.

## Zobacz także:
- Dokumentacja Arduino na temat funkcji `substring()`: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring)