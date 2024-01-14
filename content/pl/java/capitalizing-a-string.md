---
title:                "Java: Zamiana na wielkie litery ciągu znaków"
simple_title:         "Zamiana na wielkie litery ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w programowaniu musimy zmieniać teksty na wielką lub małą literę w zależności od naszych potrzeb. Jedną z najczęstszych operacji jest kapitalizacja, czyli zmiana pierwszej litery tekstu na wielką. W tym artykule dowiesz się, dlaczego jest to ważne i jak to zrobić w języku Java.

## Jak to zrobić

Aby kapitalizować tekst w Java, możemy użyć metody `toUpperCase()` lub `toLowerCase()` dostępnej dla obiektów typu `String`. Zobaczmy przykład:

```Java
String name = "jan kowalski";

System.out.println(name.toUpperCase()); // wynik: JAN KOWALSKI
System.out.println(name.toLowerCase()); // wynik: jan kowalski
```

W powyższym przykładzie widzimy, że metoda `toUpperCase()` zmienia cały tekst na wielkie litery, a `toLowerCase()` na małe. Ale co zrobimy, jeśli chcemy tylko pierwszą literę zmienić na wielką? W tym przypadku możemy skorzystać z metody `substring()` oraz `toUpperCase()`, aby wyciąć pierwszą literę i zmienić ją na wielką. Zobaczmy:

```Java
String name = "jan kowalski";

String firstName = name.substring(0, 1).toUpperCase() + name.substring(1);

System.out.println(firstName); // wynik: Jan kowalski
```

W powyższym przykładzie wykorzystaliśmy metodę `substring()` do wycięcia pierwszej litery, a następnie wykorzystaliśmy `toUpperCase()` do jej zmiany na wielką. Następnie do pierwotnego tekstu dokleiliśmy zmienioną literę, aby uzyskać pożądany efekt.

## Deep Dive

Operacja kapitalizacji jest często stosowana w programowaniu ze względu na estetykę lub wymagania projektowe. W języku Java możemy również skorzystać z metody `capitalize()` dostępnej dla obiektów typu `String`, która zmienia tylko pierwszą literę na wielką, a pozostałe zostawia bez zmian. Zobaczmy:

```Java
String name = "jan kowalski";

System.out.println(name.capitalize()); // wynik: Jan kowalski
```

Warto również zwrócić uwagę na to, że metody `toUpperCase()` oraz `toLowerCase()` są przydatne nie tylko przy kapitalizacji tekstu, ale także przy porównywaniu ciągów znaków. Jest to szczególnie przydatne przy walidacji danych wejściowych w aplikacjach.

## Zobacz też
- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [How to Capitalize Strings in Java](https://www.baeldung.com/java-uppercase-first-letter)
- [Learn Java Online - Strings](https://www.learnjavaonline.org/en/Strings)