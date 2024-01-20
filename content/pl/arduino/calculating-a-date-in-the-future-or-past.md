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

## Co i Dlaczego?
Obliczanie daty w przyszłości lub przeszłości to proces określania konkretnych dat z przyszłości lub przeszłości, znając obecną datę i ilość dni do dodania lub odjęcia. Programiści robią to, gdy magazynują lub analizują dane związane z czasem.

## Jak to zrobić:
W Arduino, możemy to zrobić za pomocą biblioteki TimeLib. Oto prosty przykładowy kod:

```Arduino
#include <TimeLib.h>

void setup(){        
  setTime(15, 45, 0, 4, 11, 2021); //ustawienie aktualnego czasu i daty 
  Serial.begin(9600);
}

void loop(){  
  time_t future_date = now() + 7 * SECS_PER_DAY; //obliczenie daty za 7 dni
  Serial.print(day(future_date));
  Serial.print(".");
  Serial.println(month(future_date));
  delay(1000);
}
```
Tym kodem obliczamy datę za 7 dni od bieżącej daty. W wyjściu zobaczymy datę za tydzień od naszej pierwotnie ustawionej daty.

## Głębiej
Za obliczanie dat w przyszłości lub przeszłości zwykle odpowiada wbudowana biblioteka lub funkcja w danym języku programowania. W Arduino korzystamy z biblioteki TimeLib, która jest łatwo dostępna i znacząco ułatwia tę czynność.

Jednak w kontekście historycznym funkcjonalność tą czesto trzeba bylo implementować samemu. Dopiero pojawienie się konkretnych bibliotek ułatwiło ten proces. 

Alternatywą dla TimeLib w Arduino może być na przykład biblioteka RTClib, która także umożliwia operacje na datach. Wybór między jedną a drugą zależy od konkretnych wymagań projektu, jak na przykład ilość dostępnej pamięci w mikrokontrolerze.

## Zobacz też
1. [Dokumentacja TimeLib](https://www.pjrc.com/teensy/td_libs_Time.html)
2. [Dokumentacja RTClib](https://adafruit.github.io/RTClib/html/index.html)
3. [Instrukcja Arduino o czasie i dacie](https://www.arduino.cc/reference/en/libraries/rtc/)