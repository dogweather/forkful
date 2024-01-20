---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Interpolacja napisów to proces ścieżkowy, który pozwala na osadzanie wartości zmiennych bezpośrednio w napisach. Programiści robią to dla wygody, żeby łatwiej zrozumieć i zwiększyć czytelność kodu.

## Jak to zrobić:

Warto pamiętać, że Java nie obsługuje bezpośrednio interpolacji napisów, jak niektóre inne języki, ale wykorzystuje tzw. formatowanie Stringów. Przykładowo:

```Java
String name = "Jan";
String greeting = String.format("Cześć, %s!", name);
System.out.println(greeting); // Wypisuje: Cześć, Jan!
```

## Dogłębne informacje:

Historycznie, interpolacja napisów nie była natywnie obsługiwana przez Jave. Java wprowadziła formatowanie Stringów w Jave 1.5, które służy jako alternatywa dla interpolacji. Jeśli chodzi o implementację, String.format() korzysta pod spodem z klasy Formatter, która tłumaczy format na konkretne wartości.

Inne języki, takie jak Python, JavaScript czy Kotlin, obsługują interpolację napisów natywnie, co umożliwia bardziej eleganckie i zwięzłe formatowanie.

```Java
// Przykład w Kotlinie
val name = "Jan"
val greeting = "Cześć, $name!"
println(greeting) // Wypisuje: Cześć, Jan!
```

## Zobacz też:

1. Dokumentacja Oracle na temat formatowania napisów: [https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html)
2. Porównanie interpolacji napisów w różnych językach: [https://www.baeldung.com/string-interpolation-java](https://www.baeldung.com/string-interpolation-java)