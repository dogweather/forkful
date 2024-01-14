---
title:    "Java: Znajdowanie długości ciągu znaków"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego warto dowiedzieć się, jak znaleźć długość ciągu znaków w Javie? Wiele programów, zwłaszcza w środowisku programistycznym, wymaga określenia długości ciągu znaków. Wiedza ta jest również niezbędna w wielu algorytmach i operacjach na ciągach, dlatego warto poznać sposób na jej znalezienie.

# Jak To Zrobić

Aby znaleźć długość ciągu znaków w Javie, możemy skorzystać z metody `length()` klasy `String`. Przykładowy kod wyglądałby następująco:

```java
String word = "Hello World!";
int length = word.length();
System.out.println("Długość wyrazu 'Hello World!' to " + length + " znaków.");
```

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```
Długość wyrazu 'Hello World!' to 12 znaków.
```

Możemy również obliczyć długość ciągu znaków wprowadzonego przez użytkownika, używając metody `length()` na obiekcie `Scanner`:

```java
Scanner input = new Scanner(System.in);
System.out.print("Wprowadź wyraz: ");
String word = input.next();
int length = word.length();
System.out.println("Długość wybranego wyrazu to " + length + " znaków.");
```

Tym razem otrzymamy wynik zależny od wprowadzonego przez nas wyrazu.

# Deep Dive

Metoda `length()` zwraca liczbę znaków w ciągu, uwzględniając również spacje i znaki specjalne. Zwraca ona wartość typu `int`, więc nie możemy jej użyć na ciągach znaków, których długość przekroczy típ `int`. W takiej sytuacji możemy skorzystać z metody `length()` klasy `StringBuffer`, która zwraca wartość typu `long`, co pozwala na obliczenie długości znaków nawet w bardzo długich ciągach.

# Zobacz także

* [Dokumentacja Java - metoda length](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#length())
* [Przykłady użycia metody length w Javie](https://www.java67.com/2017/02/how-to-find-length-of-string-in-java-with-example.html)
* [Poradnik dla początkujących - długość ciągu znaków w Javie](https://www.w3schools.com/java/java_ref_string.asp)