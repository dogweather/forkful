---
title:                "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak policzyć długość ciągu znaków w programowaniu Arduino? Jest to ważna umiejętność, ponieważ pozwala na efektywne zarządzanie pamięcią i zmiennymi. Dzięki temu blogowi dowiesz się, dlaczego jest to istotne oraz jak to zrobić.

## Jak To Zrobić

```Arduino
String myString = "Hello World";
int length = myString.length();
Serial.println(length); // Output: 11
```

W tym powyższym przykładzie, zmienna `myString` przechowuje ciąg znaków "Hello World". Następnie, używając wbudowanej funkcji `length()`, przypisujemy jej długość do zmiennej `length`. Wyświetlamy tę wartość na monitorze szeregowym za pomocą funkcji `println()`. Wynikiem powinno być "11", ponieważ jest to liczba znaków w ciągu "Hello World".

Możesz również użyć pętli `for` do przeiterowania przez wszystkie znaki w ciągu i zliczenia ich ilości.

```Arduino
String myString = "This is a test";
int counter = 0;

for (int i = 0; i < myString.length(); i++) {
  counter++;
}

Serial.println(counter); // Output: 14
```

W powyższym przykładzie, za pomocą pętli `for` iterujemy przez każdy znak w ciągu `myString` i zwiększamy zmienną `counter` o jeden przy każdym przejściu. Ostatecznie, przypisujemy wartość `counter` do długości ciągu i wyświetlamy ją na monitorze szeregowym.

## Deep Dive

Podczas liczenia długości stringa, istnieje kilka czynników, które należy wziąć pod uwagę. Po pierwsze, pamiętaj, że funkcja `length()` zwraca *liczbę* znaków, a nie *indeks* ostatniego znaku. Warto również zauważyć, że funkcja ta zlicza zarówno litery, cyfry, jak i znaki specjalne. Jeśli chcesz zliczyć tylko litery, musisz użyć funkcji `isAlpha()`.

Dodatkowo, warto wiedzieć, że ciągi znaków mogą być puste lub nieprawidłowe, co może wpłynąć na wynik funkcji `length()`.

## Zobacz też

- [Dokumentacja Arduino o funkcji length()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- [Tutorial: Praca z ciągami znaków w Arduino](https://randomnerdtutorials.com/arduino-string-processing-functions-syntax/)
- [Poradnik: Zarządzanie pamięcią w Arduino](https://maker.pro/arduino/tutorial/how-to-manage-memory-on-arduino)