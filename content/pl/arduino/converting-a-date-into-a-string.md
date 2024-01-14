---
title:                "Arduino: Konwertowanie daty na ciąg znaków"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z Arduino, chcesz być w stanie wyświetlać daty w swoich projektach. Konwertowanie daty na ciąg znaków jest niezbędne do wyświetlenia jej na ekranie lub przesłania przez komunikację szeregową.

## Jak to zrobić?

Aby przekonwertować datę na ciąg znaków, musisz użyć funkcji `sprintf()`. Poniżej przedstawiono przykładowy kod i wynik wyjściowy:

```Arduino
#include <Time.h>

void setup() {
  // Inicjalizacja połączenia szeregowego
  Serial.begin(9600);

  // Ustawienie czasu
  setTime(11, 30, 0, 12, 7, 2021); // godzina, minuty, sekundy, dzień, miesiąc, rok

  // Konwersja daty na ciąg znaków
  char data[11];
  sprintf(data, "%02d/%02d/%04d", day(), month(), year());

  // Wyświetlenie wyniku w monitorze szeregowym
  Serial.println(data);
}

void loop() {
  // Pętla główna
}
```

Wynik:

```
12/07/2021
```

W powyższym przykładzie użyliśmy funkcji `sprintf()` do sformatowania daty z obiektu `Time` na ciąg znaków i przypisaliśmy go do `char` o nazwie `data`. Następnie wyświetliliśmy ten ciąg w monitorze szeregowym za pomocą funkcji `Serial.println()`.

## Deep Dive

Funkcja `sprintf()` jest częścią biblioteki `stdio.h` i używana jest do formatowania ciągów znaków. W przykładzie powyżej użyta została instrukcja formatowania `%02d/%02d/%04d`, która oznacza, że pierwsza liczba będzie wyświetlana na dwa miejsca, a jeśli będzie krótsza, to będzie uzupełniona zerami (np. 1 stanie się 01). Ten sam format jest stosowany dla kolejnych liczb (miesiąc i rok) i dodatkowo dla roku używamy `%04d`, aby zawsze mieć cztery cyfry.

Poza tym formatem, istnieje wiele innych instrukcji, które mogą być użyte w funkcji `sprintf()`. Możesz przeczytać więcej na ten temat w [dokumentacji](https://www.cplusplus.com/reference/cstdio/printf/) języka C++.

## Zobacz także

- [Dokumentacja biblioteki Time](https://www.arduino.cc/en/reference/time)
- [Przewodnik po funkcji sprintf()](https://www.cplusplus.com/reference/cstdio/printf/) (język C++)
- [Rozszerzenie funkcji sprintf() dla Arduino](https://github.com/krzychb/Extsprintf)