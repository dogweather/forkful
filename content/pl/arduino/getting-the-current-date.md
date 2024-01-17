---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Arduino: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting the Current Date in Arduino

Często w programowaniu konieczne jest pobranie aktualnej daty i godziny. W przypadku Arduino może to być przydatne przy tworzeniu systemów zegarowych, rejestracji danych czy zarządzania czasem wykonania pewnych czynności. W tym artykule dowiesz się, jak pobrać aktualną datę w Arduino i dlaczego programiści to robią.

## Co i dlaczego?

Pobieranie aktualnej daty polega na odczytaniu informacji o dniu, miesiącu, roku i godzinie w danym momencie. Programiści wykorzystują tę funkcjonalność do śledzenia czasu, wykonywania określonych zadań w konkretnych momentach lub do rejestracji danych ze znacznikiem czasowym.

## Jak to zrobić?

W Arduino do pobrania aktualnej daty służy funkcja ```now()``` z biblioteki Time. Można ją wykorzystać do zapisania aktualnej daty do zmiennej lub jej wyświetlenia na ekranie. Przykład użycia:

```arduino
time_t currentTime = now(); // zapisanie aktualnej daty do zmiennej
Serial.println(year(currentTime)); // wyświetlenie aktualnego roku
```

Wynik będzie wyglądał podobnie do tego:

```
2021
```

## Głębsza analiza

Pobieranie aktualnej daty na Arduino jest możliwe dzięki zastosowaniu biblioteki Time, która dostarcza funkcjonalności związanych z obsługą czasu. Jest ona dostępna jako standardowa biblioteka w środowisku Arduino lub można ją pobrać z oficjalnej strony internetowej. Alternatywnie, można samodzielnie napisać funkcję do odczytu aktualnego czasu, korzystając ze zmiennych systemowych dostępnych w Arduino.

## Zobacz też

- [Dokumentacja bibilioteki Time w Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Oficjalna strona biblioteki Time](https://github.com/PaulStoffregen/Time)
- [Przykład pobierania aktualnej daty na Arduino](https://www.circuitbasics.com/arduino-getting-current-date-time/)