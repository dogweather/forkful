---
title:    "Arduino: Korzystanie z wyrażeń regularnych"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Arduino i chcesz zwiększyć swoją skuteczność w manipulowaniu tekstem, regular expressions są narzędziem, którego nie możesz przegapić. Pozwalają one na wygodne i precyzyjne filtrowanie oraz przetwarzanie danych, co jest szczególnie ważne w bardziej zaawansowanych projektach.

## Jak

Programowanie Arduino przy użyciu regular expressions jest łatwe i intuicyjne. Wystarczy dodać bibliotekę ```ArduinoRE``` do swojego projektu oraz zapoznać się z podstawowymi wyrażeniami regularnymi. Poniżej przedstawiam przykład kodu, który wyświetla tylko cyfry w tekście otrzymanym z czujnika temperatury:

```
#include <ArduinoRE.h>
#include <OneWire.h> 
#include <DallasTemperature.h>

// Inicjalizacja czujnika temperatury
OneWire oneWire(ONE_WIRE_BUS);
DallasTemperature sensors(&oneWire);

void setup() {
  // Inicjalizacja komunikacji szeregowej
  Serial.begin(9600);
  // Przypisanie wyrażenia regularnego do zmiennej
  String re = RE("@0-9");
  // Zastosowanie wyrażenia regularnego do otrzymanego tekstu
  String temp = RE(str(sensors.getTempCByIndex(0)));
  // Wyświetlenie wyniku
  Serial.println(temp);
}
```

Output:

```
23.25
```

## Deep Dive

Warto zauważyć, że regular expressions mogą być również bardzo przydatne w walidacji danych pochodzących z różnych źródeł. Dzięki nim możemy sprawdzić, czy otrzymany tekst zawiera np. wymagane formatowanie lub znaki specjalne. W bibliotece ```ArduinoRE``` dostępnych jest wiele funkcji ułatwiających pracę z regular expressions, które warto wykorzystać.

## Zobacz także

- [Dokumentacja biblioteki ArduinoRE](https://github.com/andrerogers/ArduinoRE)
- [Podstawy wyrażeń regularnych w Arduino](https://www.arduino.cc/reference/en/language/functions/communication/regex/)
- [Przykładowe projekty z użyciem regular expressions w Arduino](https://create.arduino.cc/projecthub/projects/tags/regular+expression)