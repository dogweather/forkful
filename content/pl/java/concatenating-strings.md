---
title:                "Java: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto połączyć ze sobą stringi w programowaniu?

Połączenie ze sobą wielu stringów jest niezbędne w wielu aplikacjach. Może to być przydatne przy tworzeniu wiadomości, adresów e-mail, dokumentów itp. Funkcja ta jest szczególnie przydatna, gdy mamy do czynienia z dynamicznymi danymi, które zmieniają się w zależności od użytkownika lub sytuacji.

## Jak to zrobić?

Łączenie stringów w języku Java jest bardzo proste i wymaga wykorzystania operatora "+" lub metody "concat()". Przykładowy kod wyglądałby następująco:

```java
String str1 = "Witaj";
String str2 = "świecie!";
String str3 = str1 + str2;
System.out.println(str3);
```

Output: "Witaj świecie!"

Możemy również użyć metody "concat()" w następujący sposób:

```java
String name = "Anna";
String surname = "Kowalska";
String fullName = name.concat(" ").concat(surname);
System.out.println(fullName);
```

Output: "Anna Kowalska"

Pamiętajmy, że operator "+" automatycznie konwertuje inne typy danych na typ String, co oznacza, że możemy połączyć ze sobą nie tylko dwie zmienne typu String, ale również np. liczbę z tekstem.

## Deep Dive

Podczas łączenia stringów warto pamiętać o tym, że operacja ta nie jest wykonywana na oryginalnych zmiennych, a jedynie tworzy nowy obiekt zawierający połączenie tych stringów. W przypadku gdy mamy do czynienia z większą ilością stringów, lepiej użyć klasy StringBuilder lub StringBuffer, które są bardziej wydajne.

Innym ważnym aspektem jest obsługa znaków specjalnych. Jeśli chcemy, aby znaki takie jak "\n" czy "\t" były poprawnie wyświetlane, powinniśmy użyć metody "replace()" lub "replaceAll()".

## Zobacz także

- [Java String concatenation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java StringBuilder and StringBuffer](https://www.baeldung.com/java-string-builder-string-buffer)
- [Metoda concat() w Java](https://www.javatpoint.com/java-string-concat)