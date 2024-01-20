---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie długości ciągu oznacza po prostu liczenie liczby znaków w tym ciągu. Programiści robili to z różnych powodów, jak na przykład do kontrolowania wielkości danych wejściowych lub do sterowania iteracją pętli.

## Jak to zrobić:

```Arduino 
String mojCiąg = "Cześć, świecie!";
int dlugosc = mojCiąg.length();
Serial.begin(9600);
Serial.println(dlugosc);
```
W tym przykładzie, wpisane wyjście wyniesie "16", co jest długością ciągu "Cześć, świecie!".

## Głębsze Perspektywy

Znalezienie długości ciągu jest jednym z najstarszych tricków w książce programistycznym, sięgającym czasów, gdy pamięć była kosztowna i trzeba było uważnie nią zarządzać. Alternatywą dla metody ".length()" jest stworzenie własnej funkcji, która iteruje przez ciąg do momentu napotkania "null" (terminatora ciągu). Nawiasem mówiąc, to właśnie metoda ".length()" robi pod spodem. Ta metoda kompatybilna jest całkowicie z obecną wersją Arduino.

## Zobacz Również

Zaleca się zapoznanie z poniższymi źródłami:

- [Dokumentacja Arduino na temat klasy String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Tutorial o ciągach tekstu na Arduino](https://arduino.pl/kurs/jedenastyliterowe-napisy.php)
- [Różnice między C string i Arduino String](https://arduino.stackexchange.com/questions/556/what-exactly-does-this-thing-refer-to)