---
title:                "Java: Szeregowanie ciągów znaków"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w języku Java, prawdopodobnie już spotkałeś/aś się z koniecznością łączenia ze sobą ciągów znaków (ang. string concatenation). Może się wydawać to prostym zadaniem, ale warto poznać dokładniejsze informacje na ten temat, aby uniknąć błędów i zoptymalizować swój kod.

## Jak to zrobić

Aby połączyć dwa lub więcej ciągów znaków w jedną linię, w języku Java używa się operatora `+`. Przykładowy kod wyglądałby następująco:

```Java
String firstName = "Jan";
String lastName = "Kowalski";
String fullName = firstName + " " + lastName;
System.out.println(fullName);
```

Oczekiwanym wynikiem będzie wyświetlenie `Jan Kowalski` na konsoli.

W przypadku, gdy chcemy połączyć więcej niż dwa ciągi znaków, można użyć metody `concat()` z klasy `String`. Przykładowy kod wyglądałby tak:

```Java
String firstName = "Jan";
String middleName = "Nowak";
String lastName = "Kowalski";
String fullName = firstName.concat(" ").concat(middleName).concat(" ").concat(lastName);
System.out.println(fullName);
```

Oczekiwanym wynikiem jest ponownie `Jan Nowak Kowalski`.

Warto również zwrócić uwagę na to, że w przypadku łączenia ciągów znaków z liczbami, konieczne jest wykorzystanie metody `toString()` w celu zamiany liczby na ciąg znaków.

## Deep Dive

Istnieją pewne rzeczy, o których warto pamiętać, aby uniknąć błędów i zoptymalizować swój kod przy łączeniu ciągów znaków w języku Java:

- Operator `+` działa po lewej stronie od prawej, dlatego lepiej nie używać go do łączenia dużej ilości ciągów znaków.
- Jeśli zamieniamy wiele razy wartość ciągu znaków, lepiej użyć `StringBuffer` lub `StringBuilder` zamiast operatora `+`, ponieważ jest to bardziej wydajne.

## Zobacz również

- Dokumentacja języka Java na temat string concatenation: https://docs.oracle.com/javase/tutorial/java/data/strings.html
- Porównanie wydajności String, StringBuffer i StringBuilder: https://www.javatpoint.com/StringBuilder-vsStringBuffer