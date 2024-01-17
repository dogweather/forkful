---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Arduino: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości jest procesem wykorzystywanym przez programistów, aby ustalić odpowiednie daty dla swoich projektów. Jest to szczególnie przydatne w przypadku tworzenia kalendarzy lub automatycznych harmonogramów zadań.

# Jak to zrobić:
W programowaniu Arduino można użyć funkcji ```millis()``` do uzyskania aktualnego czasu w milisekundach. Możemy również skorzystać z funkcji ```delay()```, aby wykonać odstęp czasu. Aby ustawić datę w przyszłości lub przeszłości, musimy dodać lub odjąć odpowiednią ilość milisekund do lub od aktualnego czasu.

Przykład:
```
unsigned long currentTime = millis(); // uzyskanie aktualnego czasu w milisekundach
unsigned long futureTime = currentTime + (1000 * 60 * 60 * 24); // dodanie jednego dnia (1000 milisekund * 60 sekund * 60 minut * 24 godziny)
unsigned long pastTime = currentTime - (1000 * 60 * 60 * 24); // odejmowanie jednego dnia
```

Przykładowy wynik:
```
Aktualny czas w milisekundach: 1573723584
Czas za jednym dniem: 1573813584
Czas za jednym dniem w formacie hh:mm:ss: 09:06:24
Czas za jednym dniem w formacie dd/mm/yyyy: 15/11/2019
```

# Głębsza analiza:
Istnieje wiele funkcji i bibliotek dostępnych w Arduino, które pomagają w obliczaniu dat w przyszłości lub przeszłości. Możemy na przykład użyć biblioteki RTC, aby uzyskać dokładny czas z modułu zegara rzeczywistego. Istnieją również inne metody obliczania dat w oparciu o bieżący czas lub wykorzystujące funkcje języka programowania, takie jak ```day()```, ```month()``` czy ```year()```.

# Zobacz też:
- Dokumentacja Arduino dotycząca funkcji ```millis()```: https://www.arduino.cc/reference/en/language/functions/time/millis/
- Dokumentacja Arduino dotycząca funkcji ```delay()```: https://www.arduino.cc/reference/en/language/functions/time/delay/
- Biblioteka RTC dostępna w Arduino IDE: https://github.com/adafruit/RTClib