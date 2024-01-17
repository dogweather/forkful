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

Co to jest i dlaczego to robić?

Znalezienie długości ciągu znaków (string) to po prostu znalezienie liczby znaków w danym ciągu. Programiści często muszą to robić, ponieważ jest to niezbędne przy manipulowaniu i przetwarzaniu danych tekstowych.

Jak to zrobić:

```Java
String s = "Hello World!";
int length = s.length();

System.out.println(length); // Output: 12
```

Głębsze wyjaśnienie:
 
1. Kontekst historyczny: Znalezienie długości ciągu znaków było jedną z podstawowych czynności w języku programowania C, a później także w Javie. Pozwalało to programistom na sprawdzanie czy dany ciąg jest pusty, a także na iterowanie przez dany ciąg znaków.

2. Alternatywy: W Javie istnieje także metoda ```String.length()```, która działa dokładnie w ten sam sposób. Jednak wyliczenie długości ciągu znaków za pomocą tej metody jest bardziej wydajne niż wywołanie ```String.length()```, ponieważ nie wymaga tworzenia obiektu typu String.

3. Szczegóły implementacji: W Javie, długość ciągu znaków jest domyślnie przechowywana jako liczba całkowita oznaczająca liczbę znaków. W przypadku ciągów o długości większej niż 2GB, używana jest zmienna typu long.

Zobacz również:
- [Java String Length Tutorial](https://www.w3schools.com/java/java_strings_length.asp)
- [String length() method in Java](https://www.geeksforgeeks.org/string-length-method-in-java/)