---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "Arduino: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu programistycznego, musimy zmieniać lub uaktualniać pewne fragmenty tekstu. Może to być zmiana nazwy zmiennej, poprawka literówki lub aktualizacja stałej. Dlatego warto poznać metodę wyszukiwania i zastępowania tekstu, aby szybko i skutecznie dokonywać zmian w naszym kodzie.

## Jak To Zrobić

Aby wykonać wyszukiwanie i zastępowanie tekstu w programie Arduino, możemy skorzystać z funkcji ```replace()```. Przyjmuje ona dwa argumenty - tekst do wyszukania i tekst do zastąpienia. Przykładowo, jeśli chcemy zmienić nazwę zmiennej "liczba" na "wartosc", możemy użyć poniższego kodu:

```
Arduino
String text = "Ta liczba jest równa pięciu.";
text.replace("liczba", "wartosc");
// tekst po zmianie: "Ta wartosc jest równa pięciu."
```

Jeśli chcemy wielokrotnie dokonać zastąpienia tego samego tekstu, możemy wykorzystać funkcję ```replaceAll()```. Przyjmuje ona również dwa argumenty i zastępuje wszystkie wystąpienia danego tekstu w przekazanej zmiennej.

```
Arduino
String text = "Ala ma kota, kot ma Alę.";
text.replaceAll("Ala", "Jan");
// tekst po zmianie: "Jan ma kota, kot ma Jana."
```

Funkcje ```replace()``` i ```replaceAll()``` zawsze zwracają nowy ciąg tekstowy, dlatego musimy przypisać go do zmiennej, jeśli chcemy go wykorzystać w dalszej części kodu. 

## Deep Dive

Funkcje ```replace()``` i ```replaceAll()``` służą do podstawowej operacji zastępowania tekstu. Jeśli jednak potrzebujemy bardziej zaawansowanych funkcji, możemy skorzystać z biblioteki <string.h>. Zapewnia ona szereg innych funkcji do manipulacji tekstem, takich jak ```strcat()``` czy ```strncpy()```.

Funkcje te wymagają przekazania wskaźnika na tablicę znaków, a nie obiekt typu String, dlatego musimy dokonać konwersji. Możemy to zrobić za pomocą funkcji ```String()```, która przyjmuje argument typu char i zwraca obiekt typu String. Przykładowo:

```
Arduino
char myText[] = "Hello ";
String name = "John";
String result = String(myText) + name;
Serial.println(result);
// output: Hello John
```

W ten sposób możemy manipulować tekstem za pomocą funkcji z biblioteki <string.h>, a następnie skonwertować go z powrotem do obiektu typu String.

## Zobacz Również

- [Dokumentacja funkcji replace() i replaceAll()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Dokumentacja biblioteki <string.h>](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)