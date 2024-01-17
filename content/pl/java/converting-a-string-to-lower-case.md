---
title:                "Konwersja ciągu znaków do małych liter"
html_title:           "Java: Konwersja ciągu znaków do małych liter"
simple_title:         "Konwersja ciągu znaków do małych liter"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja ciągu znaków na małe litery to proces zamiany wszystkich znaków w ciągu na ich odpowiedniki w małych literach. Programiści często wykonują tę operację, ponieważ ułatwia ona porównywanie i grupowanie tekstu bez względu na wielkość liter.

## Jak to zrobić:
```java
String text = "TEkST Do KoNWoErsji";
System.out.println(text.toLowerCase());
```
Wynik: "tekst do konwersji"

## Głębsze zagłębienie:
1. Kontekst historyczny: Konwersja na małe litery jest powszechnie stosowana w językach programowania od początku istnienia informatyki. Kiedyś znaki były przechowywane w komputerze jako liczby, a konwersja na małe litery ułatwiała porównywanie i manipulację ciągami znaków.

2. Alternatywy: Istnieje kilka sposobów na dokonanie konwersji na małe litery, m.in. użycie metody `toLowerCase()` z klasy `String`, użycie metody `toLowerCase()` z klasy `Character` lub użycie pętli `for` i funkcji `toLowerCase()` z klasy `String`.

3. Szczegóły implementacji: W języku Java istnieje kilka metod konwertujących ciąg na małe litery, jednak korzystanie z metod klasy `String` jest zalecane ze względu na wygodę i czytelność kodu. Wewnętrznie metoda `toLowerCase()` wykorzystuje tabelę mapowania znaków na ich odpowiedniki w małych literach.

## Zobacz także:
- [Dokumentacja Javy - metoda toLowerCase()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Tutorialspoint - Java Basic - String toLowerCase()](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)
- [HowToDoInJava - Java String toLowerCase and toUpperCase methods](https://howtodoinjava.com/java/string/tolowercase-touppercase/)