---
title:                "Arduino: Porównywanie dwóch dat."
simple_title:         "Porównywanie dwóch dat."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Czy zdarzyło Ci się kiedyś musieć porównać dwie daty w swoim projekcie Arduino? Porównywanie dat może być przydatne w wielu różnych aplikacjach, takich jak sterowanie czasowe lub śledzenie stanu akumulatora. W tym wpisie dowiecie się, jak to zrobić przy użyciu prostej logiki programowania i biblioteki czasu.

## Jak to zrobić
W celu porównania dwóch dat na Arduino można skorzystać z funkcji dostępnych w bibliotece `Time`. Najpierw należy zainicjować obiekt `Time` i ustawić żądany czas, a następnie wykorzystać funkcję `getDateStr` do pobrania daty w formacie `Y-m-d H:i:s`. Następnie obie daty można porównać przy użyciu operatorów logicznych, takich jak `==`, `>`, `<` itp.

```
#include <Time.h>

// Inicjalizacja obiektu Time
Time timer;

// Ustawienie daty do porównania
String date1 = timer.getDateStr();
String date2 = "2021-01-01 12:00:00";

// Porównanie dat
if(date1 == date2){
  Serial.println("Obie daty są identyczne!");
}
```

Dzięki temu prostemu kodowi można sprawdzić, czy dwie daty są takie same lub którą z nich można uznać za późniejszą. Można również wykorzystać funkcję `parseDateTime` do przekształcenia daty w obiekt `TimeElements` i porównać poszczególne elementy, takie jak rok, miesiąc, dzień czy godzina.

```
#include <Time.h>

// Inicjalizacja obiektu Time
Time timer;

// Ustawienie daty do porównania
String date1 = timer.getDateStr();
String date2 = "2021-06-20 15:30:00";

// Konwersja daty w obiekt TimeElements
TimeElements time1, time2;
timer.parseDateTime(date1.c_str(), time1);
timer.parseDateTime(date2.c_str(), time2);

// Porównanie elementów daty
if(time1.Year > time2.Year){
  Serial.println("Data pierwsza jest późniejsza!");
}
```

## Pogłębiona analiza
Porównywanie dat może być bardziej skomplikowane, jeśli chcemy uwzględnić różnicę czasu i strefy czasowe. W takim przypadku warto skorzystać z funkcji `now`, która zwraca aktualny czas w formacie `time_t` i umożliwia łatwe porównywanie dat.

```
#include <Time.h>

// Inicjalizacja obiektu Time
Time timer;

// Ustawienie daty do porównania
String date1 = timer.getDateStr();
String date2 = "2021-01-01 12:00:00";

// Konwersja daty w obiekt time_t
time_t time1 = timer.parseDateTime(date1.c_str());
time_t time2 = timer.parseDateTime(date2.c_str());

// Porównanie dat
if(time1 > time2){
  Serial.println("Data pierwsza jest późniejsza!");
}
```

## Zobacz również
- Dokumentacja biblioteki Time dla Arduino: https://playground.arduino.cc/Code/Time/
- Porównywanie czasu w Arduino: https://www.arduino.cc/reference/en/language/functions/time/now/
- Przykładowe projekty z wykorzystaniem biblioteki Time: https://create.arduino.cc/projecthub/projects/tags/time