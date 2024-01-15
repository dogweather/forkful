---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Java: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego codziennego życia. Wiele razy spotykamy się z sytuacją, gdzie musimy zmienić dany tekst w naszym kodzie lub w plikach tekstowych. W takich przypadkach pomocne jest umiejętne wykorzystanie funkcji wyszukiwania i zamiany tekstu. W tym artykule dowiesz się dlaczego warto znać tę funkcjonalność oraz jak jej używać w języku Java.

## Jak to zrobić

Aby wyszukać i zamienić tekst w Javie, możemy skorzystać z dwóch głównych metod: metody `replace()` i metody `replaceAll()`. Pierwsza z nich służy do zamiany tylko pierwszego wystąpienia danego znaku lub ciągu znaków, natomiast druga zamienia wszystkie wystąpienia. Poniższy przykład ilustruje to w praktyce:

```java
String text = "Hello, World! Hello, Java!";
String newText = text.replace("Hello", "Hi");
System.out.println(newText);
// Output: Hi, World! Hi, Java!

String allText = text.replaceAll("Hello", "Hi");
System.out.println(allText);
// Output: Hi, World! Hi, Java!
```

Ważnym elementem tych metod jest to, że są one niezmienne - to znaczy, że nie zmieniają oryginalnego tekstu, tylko zwracają nowy tekst. Dzięki temu nie musimy martwić się o utratę oryginalnych danych.

Możemy także wykorzystać wyrażenia regularne do bardziej zaawansowanych wyszukiwań i zamian w tekście. Przykład użycia:

```java
String text = "Java 8, Java 11, Java 15";
String newText = text.replaceAll("Java \\d+", "Java 14");
System.out.println(newText);
// Output: Java 14, Java 14, Java 14
```

W tym przypadku wykorzystaliśmy wyrażenie regularne, które oznacza "Java" oraz po nim jedna lub więcej cyfr. Dzięki temu wszystkie wystąpienia Java wraz z liczbami zostały zastąpione nowym tekstem "Java 14".

## Głębszy wgląd

W Javie, wyszukiwanie i zamiana tekstu może też zostać zaimplementowane przez wykorzystanie klasy `StringBuilder` oraz metody `replace()` lub `replaceAll()`. Dzięki temu możemy uniknąć tworzenia wielu obiektów `String` i zmniejszyć zużycie pamięci. Przykład użycia:

```java
StringBuilder sb = new StringBuilder("Hello, Java 11!");
sb.replace(7, 10, "World");
System.out.println(sb.toString());
// Output: Hello, World 11!

sb.replaceAll("11", "14");
System.out.println(sb.toString());
// Output: Hello, World 14!
```

Pamiętajmy, że liczby w metodzie `replace()` oraz `replaceAll()` oznaczają indeks początkowego i końcowego znaku, który chcemy zastąpić. Dzięki temu możemy precyzyjnie kontrolować, które elementy tekstu chcemy zmienić.

## Zobacz też

- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Pattern Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Java StringBuilder Class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)