---
title:                "Arduino: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami może zdarzyć się, że użytkownicy wprowadzają tekst z błędnie używanymi wielkimi literami lub znakami. W takiej sytuacji może być potrzeba zamiany tych znaków na małe litery, aby uniknąć pomyłek lub niepoprawnych wyników działania programu. W tym artykule dowiesz się, jak przekonwertować string na małe litery w Arduino.

## Jak to zrobić

Aby przekonwertować string na małe litery w Arduino, należy zastosować funkcję "toLowerCase()", która jest dostępna w bibliotece "String". Poniżej przedstawiono przykładowy kod oraz wynik jego działania:

```Arduino
#include <String.h>

String str = "Hello World!";
String newStr = str.toLowerCase();
Serial.println(newStr);

```
Output: hello world!

W powyższym przykładzie, wykorzystano funkcję "toLowerCase()" do przekonwertowania stringa "Hello World!" na małe litery i wyświetlono go za pomocą funkcji "Serial.println". W ten sposób można wprowadzić dane użytkownika i upewnić się, że są one w odpowiednim formacie.

## Deep Dive

Funkcja "toLowerCase()" jest wykorzystywana do zmiany każdego znaku w stringu na małą literę. Jeśli chcesz zachować pierwszą literę jako dużą, możesz wykorzystać funkcję "toUpperCase()" na pierwszym elemencie stringa. Warto również pamiętać, że funkcja ta nie zmienia oryginalnego stringa, ale zwraca nowy string z przekonwertowanymi literami.

Inną przydatną funkcją związaną z konwersją liter jest "charAt()". Pozwala ona na dostęp do poszczególnych znaków w stringu i może być wykorzystana w połączeniu z funkcją "toLowerCase()", aby przekonwertować tylko wybrane znaki na małe litery.

## Zobacz również

1. Dokumentacja funkcji toLowerCase() (https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
2. Przykłady zastosowania funkcji toLowerCase() (https://www.arduino.cc/en/Tutorial/StringCaseChanges)
3. Przewodnik po konwersji danych w Arduino (https://blog.arduino.cc/2016/08/04/casting-and-converting-data-in-arduino/)