---
title:    "Java: Znajdowanie długości ciągu znaków"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Często jako programiści, jesteśmy zadane z różnymi zadaniami, w tym jednym - znajdowanie długości ciągu znaków, które może być użyteczne w różnych sytuacjach, takich jak walidacja danych lub manipulacja tekstem. W tym artykule omówimy, dlaczego znajdowanie długości ciągu znaków jest ważne oraz jak to zrobić w Javie.

## Jak To Zrobić

Aby znaleźć długość ciągu znaków w Javie, użyjemy metody `length()` na obiekcie typu String, która zwraca nam liczbę znaków w ciągu. Poniższy przykład pokazuje zastosowanie tej metody:

```Java
String name = "Katarzyna";
System.out.println(name.length());
```

Output: 9

Możemy również wykorzystać pętlę `for` i metodę `charAt()` aby iteracyjnie przejść przez ciąg znaków i zliczyć ich ilość. Przykład:

```Java
String hello = "Cześć!";
int count = 0;
for (int i = 0; i < hello.length(); i++) {
  char letter = hello.charAt(i);
  count++;
}
System.out.println(count);
```

Output: 6

## Deep Dive

Każdy znak w ciągu znaków ma przypisaną swoją pozycję, zwaną indeksem. Indeks jest liczony od zera, co oznacza, że pierwszy znak w ciągu będzie miał indeks 0, drugi - indeks 1, itd. Więc, długość ciągu znaków jest równa indeksowi ostatniego znaku plus jeden.

## Zobacz również

- Dokumentacja Javy dla metody `length()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--
- Poradnik na TutorialsPoint o znajdowaniu długości ciągu znaków w Javie: https://www.tutorialspoint.com/java/java_string_length.htm
- Wideo na YouTube pokazujące zastosowanie metody `length()` w Javie: https://www.youtube.com/watch?v=SdUdQZx_Q-o