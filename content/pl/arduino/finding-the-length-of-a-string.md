---
title:                "Arduino: Znajdowanie długości łańcucha znaków"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W Arduino znajdują się wiele przydatnych funkcji, które ułatwiają proces programowania mikrokontrolera. Jedną z nich jest funkcja findString(), która pozwala na znalezienie długości ciągu znaków. Jest to bardzo ważna umiejętność, ponieważ umożliwia efektywne przetwarzanie i manipulowanie tekstem.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Arduino, należy wykonać następujące kroki:

1. Zadeklaruj zmienną typu "String", która będzie przechowywać nasz ciąg znaków.
2. Wykorzystując funkcję .length(), przypisz do innej zmiennej długość naszego ciągu.
3. Wyświetl wynik za pomocą funkcji Serial.print().

```Arduino
String tekst = "Hello world";
int dlugosc = tekst.length();
Serial.print("Dlugosc ciagu to: ");
Serial.println(dlugosc);
```

W tym przykładzie zmienna "tekst" przechowuje ciąg znaków "Hello world", a zmienna "dlugosc" zawiera jego długość, czyli 11. Wynik zostaje wyświetlony na monitorze szeregowym.

## Głębsza analiza

Podczas korzystania z funkcji findString() w Arduino warto zwrócić uwagę na kilka istotnych rzeczy:

- Funkcja ta zwraca wartość typu "int" i może przyjąć jako argument inne funkcje, np. find() lub concat().
- Jeśli ciąg znaków jest pusty, funkcja zwróci wartość 0.
- Jeśli ciąg znaków zawiera znaki specjalne, np. polskie litery, może nastąpić nieprawidłowe obliczenie długości.

Dzięki znajomości tych szczegółów, możesz skutecznie wykorzystać funkcję findString() w swoich projektach i ułatwić sobie pracę z tekstem.

## Zobacz również

1. [Dokumentacja funkcji findString() w języku angielskim](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
2. [Przykłady zastosowania funkcji findString()](https://randomnerdtutorials.com/arduino-string-length-examples/)
3. [Inne przydatne funkcje w Arduino](https://blog.arduino.cc/2016/04/14/five-essential-arduino-functions-part-1-of-2/)