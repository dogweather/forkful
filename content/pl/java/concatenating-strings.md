---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Łączenie stringów to technika, dzięki której możemy łączyć dwie lub więcej sekwencji znaków w jedną sekwencję. Programiści korzystają z niej, aby ułatwić wyświetlanie i manipulację danymi tekstowymi.

## Jak to zrobić:
Podstawową techniką łączenia stringów w Java jest użycie operatora `+`. Here is a simple example:

```Java 
String sentence = "Hello, " + "World!";
System.out.println(sentence);  // Wydruk: Hello, World!
```

Alternatywnie, możemy użyć metody `concat()` klasy String:

```Java 
String sentence = "Hello, ".concat("World!");
System.out.println(sentence);  // Wydruk: Hello, World!
```

## Głębsze zrozumienie
Historia łączenia stringów w Java sięga wstecz do samego początku tego języka. Operator `+` był dostępny od pierwszej wersji i jest on nadal najczęściej stosowany.

Innym rozwiązaniem jest użycie klasy StringBuilder, zwłaszcza gdy mamy wiele operacji konkatenacji do wykonania:

```Java 
StringBuilder sb = new StringBuilder();
sb.append("Hello, ");
sb.append("World!");
System.out.println(sb.toString()); // Wydruk: Hello, World!
```

Szczegół implementacyjny do zapamiętania to fakt, że Java automatycznie przekształca operacje na `String` używające operatora `+` do użycia `StringBuilder`, więc efektywność obu technik jest porównywalna.

## Zobacz również
- [Oracle Java documentation: Strings](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Javatpoint: String concatenation in Java](https://www.javatpoint.com/string-concatenation-in-java)
- [Baeldung: A Guide to Java String Concatenation](https://www.baeldung.com/java-strings-concatenation)