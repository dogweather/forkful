---
title:    "Java: Konwersja daty na ciąg znaków"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których konwersja daty na ciąg znaków może być niezbędna w programowaniu. Na przykład, może być konieczna do wyświetlenia daty w łatwo czytelnym formacie dla użytkownika, lub do zapisania daty w bazie danych.

## Jak to zrobić

Konwersja daty na ciąg znaków jest stosunkowo prosta w języku Java. Pierwszym krokiem jest utworzenie obiektu klasy Date, który przechowuje informacje o dacie. Następnie używamy metody SimpleDateFormat, aby sformatować datę zgodnie z naszymi wymaganiami. Przykładowy kod wyglądałby następująco:

```java
Date data = new Date();
SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
System.out.println(format.format(data));
```

W powyższym przykładzie użyto formatu "dd-MM-yyyy", który wyświetla datę w formacie dzień-miesiąc-rok. Istnieje wiele innych formatów, które możemy wybrać w zależności od potrzeb. Pełna lista dostępnych formatów znajduje się w dokumentacji klasy SimpleDateFormat.

Poniżej przedstawione są przykładowe wyniki dla różnych formatów daty:

- "dd.MM.yyyy" -> 12.10.2021
- "MM/dd/yyyy" -> 10/12/2021
- "EEEE, dd MMMM yyyy" -> Tuesday, 12 October 2021

Możemy również zastosować metodę parse(), aby przekonwertować ciąg znaków na obiekt Date. Przykładowy kod wyglądałby następująco:

```java
String stringData = "2021-10-12";
DateFormat format = new SimpleDateFormat("yyyy-MM-dd");
Date data = format.parse(stringData);
```

## Głębsza analiza

Podczas konwersji daty na ciąg znaków, ważne jest, aby zachować odpowiedni format daty, ponieważ niektóre formaty mogą interpretować datę różnie. Na przykład, jeśli użyjemy jednocyfrowego formatu miesiąca ("d-M-yyyy"), to dla daty 12-10-2021 otrzymalibyśmy wynik 12-10-2021, ale dla daty 2-10-2021 otrzymalibyśmy wynik 2-10-2021. To może spowodować problemy w późniejszej pracy z datami. Dlatego ważne jest, aby wybrać odpowiedni format dla swoich potrzeb.

## Zobacz również

- [Dokumentacja klasy SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Poradnik konwersji daty na ciąg znaków w języku Java](https://www.baeldung.com/java-date-to-string-conversion)