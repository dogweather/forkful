---
title:                "Arduino: Konwersja daty na ciąg znaków."
simple_title:         "Konwersja daty na ciąg znaków."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego 
Dlaczego konwersja daty na ciąg znaków jest ważna w programowaniu Arduino?

Konwersja daty na ciąg znaków jest ważnym elementem w programowaniu Arduino, ponieważ pozwala nam wyświetlać aktualną datę lub czas na ekranie LCD, wykorzystując tylko jeden wiersz kodu. Jest to przydatne w projektach związanych z pomiarami czasu lub wyświetlaniem informacji o dacie.

## Jak to zrobić?

Aby skonwertować datę na ciąg znaków w programie Arduino, należy użyć funkcji `sprintf()` z biblioteki `time.h`. Przykładowy kod wyglądałby następująco:

```Arduino
#include <time.h> // dołączamy bibliotekę time.h

void setup() {
  // ustawiamy czas realny RTC
  // tutaj można również ustalić aktualną datę lub czas przy pomocy innych funkcji biblioteki time.h
}

void loop() {
  // tworzymy tablicę znaków, w której będzie przechowywany ciąg znaków daty
  char date_string[11]; // 11 znaków, ponieważ datę będziemy wyświetlać w formacie DD.MM.RRRR

  // pobieramy aktualną datę z RTC i konwertujemy ją na ciąg znaków
  sprintf(date_string, "%02d.%02d.%04d", day(), month(), year()); // dokładna składnia formatu można znaleźć w dokumentacji funkcji sprintf()

  // wyświetlamy datę na ekranie, na przykład na LCD
  lcd.print(date_string);

  // możemy również przypisać ciąg znaków daty do zmiennej i wykorzystać go w innej części kodu
  String currentDate = date_string;
}
```

Powyższy kod wyświetli aktualną datę, pobraną z RTC, w formacie DD.MM.RRRR. Możemy oczywiście dostosować ten format do swoich potrzeb, wyświetlając również godzinę lub wyświetlając datę w innym formacie.

## Głębszy zanurzenie

Funkcja `sprintf()` jest bardzo wszechstronna i można jej używać do konwertowania różnych zmiennych na ciągi znaków o dowolnym formacie. Możemy używać jej również do konwertowania innych wartości związanych z datą, na przykład miesiąca lub dnia tygodnia.

Do konwersji formatu daty możemy również wykorzystać funkcje biblioteki `TimeLib.h`, takie jak `hour()` czy `minute()`. W ten sposób możemy stworzyć bardziej zaawansowane wyświetlanie aktualnego czasu na ekranie LCD lub w innych częściach kodu.

## Zobacz również

- [Dokumentacja funkcji `sprintf()`](https://www.cplusplus.com/reference/cstdio/sprintf/)
- [Dokumentacja biblioteki `Time.h`](https://www.arduino.cc/en/Reference/Time)