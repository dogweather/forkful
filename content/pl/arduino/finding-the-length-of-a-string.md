---
title:    "Arduino: Obliczanie długości ciągu znaków"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto poznać długość łańcucha znaków w programowaniu Arduino? Długość łańcucha może być przydatna przy wyświetlaniu tekstu na wyświetlaczu LCD, sprawdzaniu poprawności danych wprowadzonych przez użytkownika lub przetwarzaniu danych tekstowych w ogólności.

## Jak to zrobić

Aby poznać długość łańcucha znaków w Arduino, należy użyć funkcji `length()`. Poniżej przedstawiono przykładowy kod z wykorzystaniem tej funkcji:

```Arduino
String myString = "Hello World";
Serial.println(myString.length());
```

W powyższym przykładzie, zmienna `myString` przechowuje łańcuch "Hello World". Następnie używamy funkcji `length()`, która zwraca długość tego łańcucha i wyświetlamy ją na monitorze szeregowym za pomocą funkcji `println()`.

Output: `11`

## Głębsza analiza

W przypadku programowania mikrokontrolerów, takich jak Arduino, należy pamiętać o ograniczonej ilości pamięci. Dlatego też, podczas tworzenia łańcuchów znaków zaleca się wykorzystywanie typu `String` zamiast tradycyjnego typu `char`. Typ `String` automatycznie dostosowuje swoją długość do wprowadzanego tekstu, a typ `char` wymaga wcześniejszego określenia rozmiaru, co może prowadzić do problemów z pamięcią.

Funkcja `length()` nie uwzględnia znaku końca `'\0'` w obliczaniu długości łańcucha. Oznacza to, że jeśli łańcuch zawiera 10 znaków, to jego długość zostanie zwrócona jako 10, a nie 11.

## Zobacz także

- [Dokumentacja funkcji length()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- [Porównanie typu String z typem char w Arduino](https://forum.arduino.cc/index.php?topic=3922.0)
- [Podstawy programowania w C++ dla Arduino](https://roboticsbackend.com/arduino-c-plus-plus-programming/)