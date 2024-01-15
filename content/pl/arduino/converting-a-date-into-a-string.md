---
title:                "Konwersja daty na łańcuch znaków"
html_title:           "Arduino: Konwersja daty na łańcuch znaków"
simple_title:         "Konwersja daty na łańcuch znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym narzędziem dla wielu projektów opartych na Arduino. Umożliwia ona wyświetlanie daty i czasu na ekranie LCD, zapisywanie ich w plikach lub wysyłanie ich przez sieć. Wiele urządzeń IoT również wykorzystuje konwersję daty na ciąg znaków do synchronizacji z serwerem lub bazy danych.

# Jak to zrobić

Aby przekonwertować datę na ciąg znaków w Arduino, należy użyć funkcji `sprintf ()`. Poniższy przykład demonstruje konwersję bieżącej daty na ciąg znaków w formacie "dzień-miesiąc-rok" i wyświetla ją na monitorze szeregu.

```Arduino
#include <Time.h> 
void setup() {
  Serial.begin(9600);
  setTime(18, 40, 0, 25, 5, 2021); //Ustawia aktualną datę i czas
}
 
void loop() {
  char dateStr[9];
  sprintf(dateStr, "%02d-%02d-%04d", day(), month(), year()); //Konwertuje datę na ciąg znaków
  Serial.println(dateStr); //Wyświetla datę na monitorze szeregowym
  delay(1000); //Czeka 1 sekundę
}
```
**Wynik:**
```
25-05-2021
```

Aby uzyskać pełną listę formatów daty i czasu dostępnych w funkcji `sprintf()`, można przejrzeć dokumentację biblioteki Time.

# Głębsza analiza

Funkcja `sprintf()` jest często wykorzystywana do konwersji liczbowych wartości na ciągi znaków w Arduino. W przypadku konwersji daty, funkcja ta jest szczególnie przydatna, ponieważ pozwala na manipulację formatem daty według własnych preferencji. Poza tym, Arduino nie ma wbudowanej funkcji do przekształcania daty na ciąg znaków, więc `sprintf()` staje się niezastąpionym narzędziem w tego typu operacjach.

# Zobacz też

- Dokumentacja funkcji sprintf() w bibliotece Arduino: https://www.arduino.cc/reference/en/language/functions/character-functions/sprintf/
- Tutorial o bibliotece Time w Arduino: https://www.instructables.com/Use-RCtime-From-a-Library-Into-an-Instructable/
- Poradnik o konwersji liczb na ciągi znaków w Arduino: https://randomnerdtutorials.com/arduino-data-type-conversion-functions-tostring-constrain-map/