---
title:    "Arduino: Pobieranie bieżącej daty"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego warto pobrać aktualną datę?

Pobieranie aktualnej daty w programowaniu Arduino może być niezbędne w różnych projektach. Jest to przydatne w przypadku tworzenia datowników, kalendarzy oraz innych urządzeń, które muszą uwzględniać aktualną datę. W niektórych przypadkach może być także potrzebne do synchronizacji zewnętrznego sprzętu lub serwera.

## Jak to zrobić?

Do pobrania aktualnej daty w Arduino możemy użyć funkcji `millis()`, która zwraca liczbę milisekund, które minęły od uruchomienia płytki. Dzięki temu możemy wyliczyć aktualną datę na podstawie ustalonej daty początkowej.

```Arduino
unsigned long startMillis;
unsigned long currentMillis;

void setup() {
  //ustawienie daty początkowej na sekundę
  startMillis = millis() / 1000;
}

void loop() {
  //wyliczenie aktualnej sekundy
  currentMillis = millis() / 1000 - startMillis;

  //przetworzenie na aktualną datę
  int currentYear = 2020;
  int currentMonth = 9;
  int currentDay = 1 + (currentMillis / 86400);

  //wyświetlenie na serial monitorze
  Serial.print("Dzisiejsza data:");
  Serial.print(currentDay);
  Serial.print("/");
  Serial.print(currentMonth);
  Serial.print("/");
  Serial.println(currentYear);
}

```
### Przykładowe wyjście:
```
Dzisiejsza data: 1/9/2020
```

## Wnikliwa analiza

Istnieje kilka innych sposobów na pobranie aktualnej daty w Arduino. Jednym z popularniejszych jest użycie zewnętrznych modułów RTC (Real-Time Clock), które posiadają wbudowane baterie i są w stanie wyświetlać aktualną datę niezależnie od zasilania płytki. Niektóre płytki Arduino, takie jak Arduino Nano czy Arduino Uno, posiadają także wbudowany oscylator krystaliczny, który może być użyty do wyliczenia aktualnej daty.

W przypadku bardziej zaawansowanych projektów, może być konieczne również uwzględnienie strefy czasowej oraz zmian czasu zimowego i letniego, co może być wyzwaniem w programowaniu.

## Zobacz także

- [Poradnik: Pobieranie daty i czasu z modułu RTC](https://www.whadda.com/how-to-sync-an-arduino-with-a-real-time-clock-using-rtc-module/)
- [Dokumentacja Arduino: Funkcja millis()](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Tutorial: Przetwarzanie informacji o czasie](https://www.arduino.cc/en/Tutorial/DateTime)