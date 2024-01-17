---
title:                "Porównywanie dwóch dat"
html_title:           "Arduino: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Porównywanie dwóch dat jest ważnym aspektem programowania, ponieważ pozwala nam określić relacje pomiędzy tymi datami. W przypadku Arduino, porównywanie dwóch dat może być przydatne w wielu projektach, takich jak np. automatyczne sterowanie oświetleniem w zależności od pory dnia.

## Jak to zrobić:
Arduino oferuje wiele metod do porównywania dwóch dat. Jedną z najprostszych jest użycie funkcji numerycznych dla roku, miesiąca i dnia. Przykładowy kod i jego wynik można zobaczyć poniżej:
```
#include <DateTime.h>

DateTime date1 (2020, 10, 27); // data wejściowa
DateTime date2 (2019, 10, 27); // data porównywana

if (date1.year() > date2.year()) // porównanie roku
{
  Serial.println("Rok daty pierwszej jest większy od roku daty drugiej.");
}
```

## Głębsze zanurzenie:
Porównywanie dat nie jest nowym problemem w świecie programowania. Wcześniej wykorzystywano do tego celu różnego rodzaju biblioteki i funkcje. Jednak dzięki rozwojowi technologii, Arduino oferuje teraz proste i wydajne metody do porównywania dat. Alternatywne podejście może polegać na użyciu funkcji wbudowanych w język programowania, takich jak "strcmp" dla C++.

## Zobacz również:
- Biblioteka DateTime do Arduino: https://github.com/PaulStoffregen/DateTime
- Inne sposoby na porównywanie dat w Arduino: https://forum.arduino.cc/index.php?topic=399061.0