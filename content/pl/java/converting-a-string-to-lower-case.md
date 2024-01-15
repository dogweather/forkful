---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Java: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągu znaków na małe litery jest ważnym aspektem programowania w języku Java. Pozwala ona na porównywanie i analizowanie tekstu bez uwzględniania wielkości liter, co jest niezbędne w wielu przypadkach.

## Jak to zrobić

Przede wszystkim musimy zapewnić, że nasz kod będzie miał dostęp do klasy String, która zawiera metody do manipulowania tekstem. Następnie, możemy użyć metody `toLowerCase()` na naszym ciągu znaków, aby dokonać konwersji. Przykładowy kod poniżej:

```java
String text = "HELLO WORLD";
System.out.println(text.toLowerCase());
```

To spowoduje, że na ekranie zostanie wyświetlony ciąg znaków "hello world". 

Jeśli chcemy przeprowadzić konwersję na obiekcie typu String, możemy użyć metody `toLowerCase()` na zmiennej tego typu:

```java
String text = "HELLO WORLD";
String lowercaseText = text.toLowerCase();
```

W powyższym przykładzie, zmienna `lowercaseText` będzie zawierać ciąg znaków "hello world". 

## Głębsze wyjaśnienie

W języku Java istnieją dwa sposoby na konwersję ciągu znaków na małe litery. Możemy użyć metody `toLowerCase()`, która zwraca nowy obiekt typu String zawierający wynik konwersji. Lub, możemy użyć metody `toCharArray()`, która zwraca tablicę typu char zawierającą wynik konwersji. Przykładowy kod poniżej:

```java
String text = "HELLO WORLD";

// metoda toLowerCase()
String lowercaseText = text.toLowerCase();
System.out.println(lowercaseText);

// metoda toCharArray()
char[] charArray = text.toCharArray();
for (char c : charArray) {
    if (c >= 'A' && c <= 'Z') {
        c += 32; // dodajemy 32 do kodu ASCII znaku, aby zmienić go na właściwą małą literę
    }
    System.out.print(c);
}
```

W powyższym przykładzie, wykorzystaliśmy fakt, że w ASCII, duże litery mają wyższe kody niż małe litery. Dlatego, dodając 32 do kodu ASCII znaku, zmieniamy go na odpowiednią małą literę. 

## Zobacz też

- [Dokumentacja Java String](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [Ciągi znaków w języku Java](https://www.geeksforgeeks.org/java-string/)