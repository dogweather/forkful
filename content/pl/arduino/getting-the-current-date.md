---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli masz projekt, w którym potrzebujesz ustawiać aktualną datę, poznaj metodę uzyskiwania jej w programowaniu Arduino. Z tej wiedzy możesz skorzystać, aby wyświetlić datę na ekranie lub zaprogramować określone działania na konkretny dzień.

## Jak To Zrobić

Pierwszym krokiem jest zdefiniowanie zmiennej, która przechowa aktualną datę:

```Arduino
int currentDay = day(); //zmienna przechowująca aktualny dzień
int currentMonth = month(); //zmienna przechowująca aktualny miesiąc
int currentYear = year(); //zmienna przechowująca aktualny rok
```

Następnie możemy wyświetlić datę na ekranie LCD:

```Arduino
lcd.print(currentDay + "/" + currentMonth + "/" + currentYear); //wyświetlenie daty na ekranie LCD
```

Inną przydatną funkcją jest obliczenie dnia tygodnia dla danej daty:

```Arduino
int currentWeekday = weekday(); //zmienna przechowująca aktualny dzień tygodnia
```

Możemy również ustawić określone działania na konkretny dzień, np. zaprogramować wyzwalanie alarmu w poniedziałek rano.

## Film

Aby dokładniej poznać możliwości uzyskiwania aktualnej daty w Arduino, zapraszam do obejrzenia filmu [Arduino Date and Time Tutorial by HowToMechatronics](https://www.youtube.com/watch?v=mZFpQlwkqlU).

## Wnikliwe Badanie

Arduino korzysta z biblioteki Time, która zapewnia wiele funkcji związanych z czasem i datą. Dzięki niej możemy uzyskać m.in. czas w milisekundach, sprawdzić, czy dany rok jest przestępny, czy porównywać daty. Więcej informacji o bibliotece Time można znaleźć na [stronie oficjalnej](https://www.arduino.cc/en/Reference/Time).

Możliwości Arduino w zakresie uzyskiwania aktualnej daty są szerokie i warto zapoznać się zarówno z dokumentacją, jak i skorzystać z innych materiałów dostępnych w sieci.

## Zobacz Również

- [Arduino Reference - Date and Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Instructables - Get Current Date on LCD using Arduino](https://www.instructables.com/Get-Current-Date-on-LCD-using-Arduino/)
- [Random Nerd Tutorials - Guide for DS1307 Real Time Clock (RTC) Module with Arduino](http://randomnerdtutorials.com/guide-for-ds3231-real-time-clock-rtc-with-arduino/)

Translated by [artykulix.pl](https://artykulix.pl/).