---
title:    "Arduino: Konwertowanie daty na łańcuch znaków"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędna w wielu projektach Arduino, które wymagają wyświetlania aktualnej daty lub zapisywania jej do pliku. Jest to także przydatne w przypadku tworzenia dzienników lub rejestrów zdarzeń, gdzie dokładna data jest istotnym elementem.

## Jak to zrobić

Konwersja daty na ciąg znaków w Arduino jest dość prostym procesem. Wystarczy zastosować kilka funkcji dostępnych w bibliotece "Time", która jest zwykle używana do zarządzania czasem w Arduino.

```Arduino
#include <Time.h>

// Pobierz aktualny czas
time_t currentTime = now();

// Konwertuj czas na ciąg znaków w formacie "DD/MM/YYYY"
String dateString = day(currentTime) + "/" + month(currentTime) + "/" + year(currentTime);

// Wyświetl skonwertowaną datę
Serial.println(dateString);
```

Przykładowy output:
```
21/09/2021
```

## Głębszy zanurzenie się w temat

Zanim rozpoczniesz konwertowanie daty na ciąg znaków w Arduino, musisz upewnić się, że masz ustawiony poprawny czas w module RTC lub wewnętrznych zegarach mikrokontrolera. Jeśli sposób pobierania czasu jest konfigurowalny, będziesz musiał dostosować odpowiednio funkcje konwertujące datę, aby wyświetlały poprawne wartości.

Kolejnym ważnym sposobem konwersji daty jest wykorzystanie funkcji "sprintf", która pozwala zapisywać sformatowane dane do ciągu znaków. Może to znacznie ułatwić manipulację danymi i dostosowywanie wyświetlanego formatu daty.

## Zobacz także

- Dokumentacja biblioteki Time: https://github.com/PaulStoffregen/Time
- Przewodnik po konwersji typów zmiennych w Arduino: https://www.arduino.cc/reference/pl/language/variables/conversion/