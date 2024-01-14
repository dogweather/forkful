---
title:    "Arduino: Konkatenacja ciągów"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego
Jeśli pracujesz z Arduino, mogłeś zetknąć się z sytuacją, w której potrzebowałeś połączyć kilka tekstów w jeden ciąg. Może to być przydatne, na przykład gdy chcesz wyświetlić na ekranie LCD wyrażenie zawierające wartości pomiarów z czujników. W takim przypadku wygodniej jest połączyć wszystkie te wartości w jedną linię, zamiast wyświetlać je osobno. W tej blogowej instrukcji dowiesz się, jak wykonać to zadanie za pomocą funkcji "concat()" w Arduino.

## Jak to zrobić
```Arduino
String tekst1 = "Cześć";
String tekst2 = "Arduino!";
String wynik = concat(tekst1, tekst2);
Serial.println(wynik);
```

Po wprowadzeniu powyższego kodu do swojego projektu, na monitorze szeregowym powinno pojawić się "Cześć Arduino!". W tym przykładzie zastosowaliśmy standardową bibliotekę String w celu stworzenia dwóch zmiennych typu String, zawierających odpowiednio "Cześć" i "Arduino!". Następnie wywołaliśmy funkcję "concat()", podając jako argumenty obie zmienne. Dzięki temu możemy połączyć je w jedną linię i przypisać do nowej zmiennej "wynik". Na koniec wystarczy tylko wydrukować tę zmienną za pomocą funkcji "println()" i gotowe!

## Pogłębione badanie
Funkcja "concat()" nie jest jedynym sposobem na łączenie stringów w Arduino. Można to również zrobić za pomocą operatora "+", na przykład: `String wynik = tekst1 + " " + tekst2;`. Jednak funkcja "concat()" może być przydatna, gdy zachodzi potrzeba łączenia większej liczby stringów lub gdy chcemy zachować czytelniejszy kod.

Warto również pamiętać, że w Arduino działają również inne funkcje związane z manipulacją stringami, takie jak "substring()" czy "charAt()", które mogą być przydatne przy bardziej zaawansowanych zastosowaniach.

## Zobacz również
- [Dokumentacja funkcji "concat()" w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Wprowadzenie do działania zmiennych typu String w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Przykłady wykorzystania manipulacji stringami w Arduino](https://www.instructables.com/Arduino-String-Manipulation/)