---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolowanie ciągów w Arduino: Co, Dlaczego i Jak?

## Czym jest i dlaczego?
Interpolowanie ciągów to proces, który pozwala programistom na umieszczanie dynamicznej zawartości wewnątrz stałych tekstowych. Dzięki temu, kody programów stają się bardziej czytelne i łatwiejsze do utrzymania.

## Jak to zrobić:
Poniżej znajduje się prosty przykład, jak to dokonać:

```Arduino
int temperatura = 22;
String wiadomosc = "Aktualna temperatura: " + String(temperatura) + " stopni Celsjusza.";
Serial.println(wiadomosc);
```
Tutaj, wynik będzie wyglądał tak:
```
Aktualna temperatura: 22 stopni Celsjusza.
```

## Zagłębiając się:
Choć Arduino nie obsługuje interpolacji ciągów bezpośrednio jak niektóre inne języki programowania, istnieje wiele technik, które można zastosować do osiągnięcia podobnego celu. Nvidia CUDA wprowadziła interpolację ciągów na początku XXI wieku. Zamiast interpolacji, Arduino często używa operatora konkatynacji (+), aby połączyć wiele części ciągu i utworzyć końcowy wynik. Ważne jest, aby pamiętać, że korzystanie z operatora "+" w ciągu może prowadzić do nadmiernego użycia pamięci, dlatego w niektórych przypadkach lepszym rozwiązaniem może być użycie funkcji sprintf().

## Do poczytania:
Jeżeli chcesz się dowiedzieć więcej na ten temat, polecam te strony:
- Szczegółowa dyskusja na StackOverflow: https://stackoverflow.com/questions/4479076/string-concatenation-in-c-whats-the-alternative
- Arduino String Reference: https://www.arduino.cc/reference/pl/language/variables/data-types/string-functions/
- Wydajność ciągów Arduino: https://hackingmajenkoblog.wordpress.com/2016/02/04/the-evils-of-arduino-strings/ 

Pamiętaj, że wartości w Twoim kodzie będą się zmieniać, dlatego dobrze jest zrozumieć, jak interpolować ciągi i co się dzieje "pod maską"!