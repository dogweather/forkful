---
title:    "Arduino: Łączenie ciągów znaków"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągłe łączenie ciągów tekstowych jest ważnym elementem wielu projektów w świecie Arduino. Dzięki temu można wyświetlać informacje na ekranach LCD, zapisywać dane w pamięci lub przesyłać je do innych urządzeń. Funkcja ta jest niezbędna dla zaawansowanych projektów, dlatego warto nauczyć się jej obsługi.

## Jak to zrobić

W języku Arduino teksty są przechowywane w postaci tablicy znaków. Aby połączyć dwa lub więcej tekstów, musimy wykorzystać specjalną funkcję "strcat()" (skrót od angielskich słów "string concatenate"). Poniżej znajdują się przykładowe kody, które pokażą, jak użyć tej funkcji w praktyce.

```Arduino
char tekst1[] = "Witaj";
char tekst2[] = " w świecie";
char tekst3[] = " Arduino!";
char wynik[100]; //zadeklarowanie tablicy na wynik
```
```Arduino
strcat(wynik, tekst1); //dodanie tekstu "Witaj" do tablicy wynik
strcat(wynik, tekst2); //dodanie tekstu "w świecie" do tablicy wynik
strcat(wynik, tekst3); //dodanie tekstu "Arduino!" do tablicy wynik
```
Podczas działania tego kodu, zmienna "wynik" będzie zawierać napis "Witaj w świecie Arduino!".

## Głębsze zagadnienia

Funkcja "strcat()" umożliwia również łączenie liczb i zmiennych z ciągami tekstowymi. W przypadku liczb, konieczne jest skonwertowanie ich na tekst za pomocą funkcji "itoa()", ponieważ funkcja "strcat()" działa tylko na danych typu "char". Dzięki temu możemy np. łatwo wyświetlić wynik zapisanej operacji na ekranie LCD.

Warto również wiedzieć, że w przypadku dużych napisów, operacje łączenia mogą być czasochłonne i zużywać pamięć RAM. Dlatego ważne jest, aby pamiętać o optymalizacji swojego kodu i unikać niepotrzebnego łączenia długich ciągów tekstowych.

## Zobacz także

- [Oficjalna dokumentacja funkcji strcat() w języku Arduino](https://www.arduino.cc/reference/en/language/functions/string-datatype/strcat/)
- [Przykłady użycia funkcji strcat() na stronie Arduino Forum](https://forum.arduino.cc/index.php?topic=36492.0)
- [Poradnik optymalizacji kodu dla Arduino](https://blog.arduino.cc/2015/03/17/optimizing-your-arduino-code/)