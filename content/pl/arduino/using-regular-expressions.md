---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Regularne wyrażenia (regex) to potężne narzędzie do wyszukiwania i manipulowania tekstami. Programiści korzystają z nich, by szybko znaleźć, sprawdzić lub zmienić fragmenty kodu według konkretnego wzorca.

## Jak to zrobić:
Arduino nie ma wbudowanej obsługi regex, ale możesz użyć funkcji `indexOf()`, `lastIndexOf()`, `startsWith()`, `endsWith()` i `substring()` do prostego przetwarzania tekstu. Możliwa jest także integracja z zewnętrznymi bibliotekami.
```Arduino
String data = "Temp: 24C, Hum: 60%";
int tempIndex = data.indexOf("Temp:");
int tempValueStart = data.indexOf(" ", tempIndex);
int tempValueEnd = data.indexOf("C", tempValueStart);
String temp = data.substring(tempValueStart + 1, tempValueEnd);
Serial.println("Temperatura: " + temp + " stopni Celsjusza");

int humIndex = data.indexOf("Hum:");
int humValueStart = data.indexOf(" ", humIndex);
int humValueEnd = data.indexOf("%", humValueStart);
String humidity = data.substring(humValueStart + 1, humValueEnd);
Serial.println("Wilgotność: " + humidity + "%");
```
Wyjście próbki:
```
Temperatura: 24 stopni Celsjusza
Wilgotność: 60%
```

## Głębsze spojrzenie:
Historia regex zaczyna się w latach 50. jako teoria automatów i języków formalnych. Na Arduino alternatywą dla regex może być użycie funkcji `String` lub skomplikowanej logiki z użyciem typów danych `char` i tablic. Implementacja pełnej obsługi regex wymagałaby znacznych zasobów, co jest wyzwaniem w ograniczonym środowisku mikrokontrolerów.

## Zobacz również:
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Playground: Text Parsing Library](http://playground.arduino.cc/Code/TextFinder)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
