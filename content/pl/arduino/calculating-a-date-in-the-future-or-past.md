---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "Arduino: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/chciałaś wiedzieć, jaki będzie dzień tygodnia lub data za dwa tygodnie? Może planujesz wydarzenie i chcesz mieć pewność, że będzie ono w wybrany przez Ciebie dzień? W artykule tym dowiesz się, jak używać Arduino do obliczania daty w przyszłości lub przeszłości.

## Jak to zrobić

Arduino posiada wbudowaną bibliotekę `time`, która umożliwia nam manipulowanie datą i czasem. Korzystając z funkcji `now()` możemy uzyskać aktualną datę i czas w formacie `YYYY-MM-DD HH:MM:SS`. Aby ustawić przyszłą lub przeszłą datę, musimy dodać lub odjąć odpowiednią ilość milisekund, sekund, minut, godzin, dni, miesięcy lub lat od aktualnej daty.

Przykład 1: Obliczanie daty za 2 tygodnie

```Arduino
#include <Time.h>

int daysToAdd = 14; // ilość dni do dodania, w tym przypadku 2 tygodnie
long newTime = now() + daysToAdd * 86400; // 86400 sekund w ciągu jednego dnia
tmElements_t tm; // struktura do przechowywania danych
breakTime(newTime, tm); // konwersja czasu na poszczególne elementy
// wyświetlenie daty w formacie DD.MM.YYYY
Serial.print(tm.Day);
Serial.print(".");
Serial.print(tm.Month);
Serial.print(".");
Serial.print(tmYearToCalendar(tm.Year));
```

Output: 07.10.2021 (zakładając, że jest obecnie 23.09.2021)

Przykład 2: Obliczanie daty 3 dni temu

```Arduino
#include <Time.h>

int daysToSub = 3; // ilość dni do odjęcia, w tym przypadku 3 dni
long newTime = now() - daysToSub * 86400; // 86400 sekund w ciągu jednego dnia
tmElements_t tm; // struktura do przechowywania danych
breakTime(newTime, tm); // konwersja czasu na poszczególne elementy
// wyświetlenie daty w formacie DD.MM.YYYY
Serial.print(tm.Day);
Serial.print(".");
Serial.print(tm.Month);
Serial.print(".");
Serial.print(tmYearToCalendar(tm.Year));
```

Output: 20.09.2021 (zakładając, że jest obecnie 23.09.2021)

## Dogłębna analiza

Jednostką liczenia czasu w Arduino jest milisekunda. Dlatego w przykładach powyżej mnożymy i dzielimy przez 86400 (24 godziny * 60 minut * 60 sekund) w celu uzyskania odpowiedniej ilości sekund.

Możemy także użyć funkcji `hour()`, `minute()`, `second()` do pobrania aktualnych godzin, minut i sekund z daty. Możemy również użyć funkcji `day()`, `month()`, `year()` do pobrania aktualnego dnia, miesiąca i roku.

Jeśli chcemy wyświetlać daty w innych formatach, na przykład "MM/DD/RRRR", musimy odpowiednio zmodyfikować wyświetlane wartości ze struktury `tm`.

## Zobacz także

- Oficjalna dokumentacja biblioteki `time` w Arduino: https://www.arduino.cc/en/reference/time
- Przykładowy projekt wykorzystujący manipulację datą i czasem w Arduino: https://create.arduino.cc/projecthub/vondrak/create-a-clock-with-arduino-daccfe