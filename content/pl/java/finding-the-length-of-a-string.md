---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Java: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znajdowanie długości ciągu znaków jest ważną umiejętnością w programowaniu w Javie. Pozwala ona na manipulację i analizę tekstu oraz tworzenie funkcjonalności, takich jak sprawdzanie poprawności wprowadzonych danych czy generowanie unikalnych identyfikatorów.

## Jak to zrobić

```Java
String str = "Cześć, Java!";
int length = str.length();
System.out.println("Długość ciągu znaków \"" + str + "\": " + length);
```

**Output:**
Długość ciągu znaków "Cześć, Java!": 11

W powyższym kodzie użyliśmy metody `length()`, dostępnej dla obiektów typu `String`, aby zwrócić długość ciągu. Warto zauważyć, że metoda ta liczy również znaki białe, takie jak spacje czy tabulatory. Jeśli chcesz wykluczyć te znaki, możesz najpierw użyć metody `trim()`. Przykładowe wykorzystanie:

```Java
String str = "   Cześć, Java!   ";
int length = str.trim().length();
System.out.println("Długość ciągu znaków \"" + str + "\": " + length);
```

**Output:**
Długość ciągu znaków "Cześć, Java!": 11

## Deep Dive

W Javie ciągi znaków są traktowane jako obiekty, a nie jako tablice znaków. Dlatego też nie możemy używać operatora indeksowania `[]`, aby uzyskać dostęp do poszczególnych znaków. Zamiast tego, musimy skorzystać z metody `charAt()`:

```Java
String str = "Hello, Java!";
char firstChar = str.charAt(0);
System.out.println("Pierwszy znak ciągu \"" + str + "\": " + firstChar);
```

**Output:**
Pierwszy znak ciągu "Hello, Java!": H

Dodatkowo, długość ciągu znaków w Javie jest ograniczona do wartości maksymalnej typu `int`, czyli 2,147,483,647. Jeśli chcesz sprawdzić długość ciągu większą niż ta wartość, możesz skorzystać z metody `length()` klasy `BigInteger`:

```Java
String longStr = "This is a very long string";
BigInteger bigInteger = BigInteger.valueOf(longStr.length());
System.out.println("Długość ciągu znaków \"" + longStr + "\": " + bigInteger.toString());
```

**Output:**
Długość ciągu znaków "This is a very long string": 24

## Zobacz też

- [Dokumentacja Java - metody klasy String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#length())
- [Tutorial o długości ciągu znaków w Javie](https://www.javatpoint.com/java-string-length)