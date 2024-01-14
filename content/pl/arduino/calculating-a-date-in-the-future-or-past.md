---
title:    "Arduino: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Dlaczego obliczać datę w przyszłości lub przeszłości?

Obliczanie dat w przyszłości lub przeszłości przydatne jest w różnych projektach Arduino. Może być to potrzebne do wyświetlania daty i godziny na wyświetlaczu LCD lub do planowania wykonywania jakiejś akcji w określonym terminie. Dzięki temu możemy w pełni wykorzystać możliwości naszego mikrokontrolera i stworzyć bardziej zaawansowane projekty.

# Jak to zrobić?

Aby obliczyć datę w przyszłości lub przeszłości, należy najpierw zdefiniować zmienną typu `DateTime`. Następnie, za pomocą funkcji `DateTime::now()` pobierzemy aktualną datę i godzinę. Następnie możemy określić, o ile dni chcemy przesunąć datę i użyć funkcji `DateTime::addDays()` lub `DateTime::subtractDays()`. Po przesunięciu daty, możemy wyświetlić ją na wyświetlaczu lub wykorzystać w inny sposób.

```Arduino
#include <DateTime.h>

DateTime dzisiaj = DateTime::now();
DateTime jutro = dzisiaj.addDays(1);
Serial.print("Jutrzejsza data: ");
Serial.print(jutro.year());
Serial.print("-");
Serial.print(jutro.month());
Serial.print("-");
Serial.println(jutro.day());
```

W powyższym przykładzie obliczamy datę jutrzejszą i wyświetlamy ją na porcie szeregowym.

# Głębsze zagadnienia

Obliczanie daty w przyszłości lub przeszłości może być nieco bardziej skomplikowane, jeśli chcemy uwzględnić różnego rodzaju wyjątki, takie jak przestępne lata czy różnego rodzaju kalendarze. W takich przypadkach, zalecamy skorzystanie z gotowych bibliotek, które ułatwiają obliczenia związane z datami.

# Zobacz także

- [Biblioteka DateTime](https://github.com/PaulStoffregen/Time)
- [Biblioteka RTClib](https://github.com/adafruit/RTClib)
- [Poradnik o pracy z datami i godzinami w Arduino](https://www.arduino.cc/en/tutorial/calendar)