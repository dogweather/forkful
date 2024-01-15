---
title:                "Zapisywanie ciągu znaków"
html_title:           "Java: Zapisywanie ciągu znaków"
simple_title:         "Zapisywanie ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu występuje potrzeba zmiany wielkości liter w ciągach znaków. Może być to przydatne przy porównywaniu lub wyświetlaniu tekstu. Własciwa obsługa zmiany wielkości liter jest istotna dla poprawnego funkcjonowania programu.

## Jak to zrobić

Aby zamienić wszystkie litery w ciągu znaków na duże lub małe, wystarczy wykorzystać wbudowane metody klasy `String`.

Przykład dla zamiany na duże litery:
```Java
String string1 = "to jest przykładowy tekst";
System.out.println(string1.toUpperCase());
```
**Output:** TO JEST PRZYKŁADOWY TEKST

Przykład dla zamiany na małe litery:
```Java
String string2 = "TO JEST PRZYKŁADOWY TEKST";
System.out.println(string2.toLowerCase());
```
**Output:** to jest przykładowy tekst

Jeśli chcemy zmienić jedynie pierwszą literę w ciągu na dużą, należy wykorzystać klasę `StringBuilder` oraz metodę `charAt()` i `toUpperCase()`.

Przykład:
```Java
String string3 = "to jest przykładowy tekst";
StringBuilder stringBuilder = new StringBuilder(string3);
stringBuilder.setCharAt(0, Character.toUpperCase(stringBuilder.charAt(0)));
System.out.println(stringBuilder.toString());
```
**Output:** To jest przykładowy tekst

## Deep Dive

W przypadku gdy mamy do czynienia z większą ilością tekstu, przydatne może być użycie metody `split()` do podzielenia tekstu na tablicę wyrazów i następnie połączenie ich z odpowiednio zmienionymi pierwszymi literami przy użyciu pętli.

Warto również pamiętać, że metody `toUpperCase()` i `toLowerCase()` działają tylko na literach angielskiego alfabetu. W przypadku innych języków mogą pojawić się problemy ze zmianą wielkości liter.

## Zobacz również
- [Java - klasa String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java - klasa StringBuilder](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)
- [Metody manipulacji łańcuchami znaków w Java](https://www.javatpoint.com/java-string-methods)