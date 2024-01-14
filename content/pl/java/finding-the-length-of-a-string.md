---
title:                "Java: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdego dnia korzystamy z różnych funkcji w języku programowania Java, ale nie zawsze zastanawiamy się dlaczego są one istotne. Jedną z takich funkcji jest znajdowanie długości napisu. Dlaczego jest to ważne? Dzięki temu możemy w łatwy sposób określić ilość znaków w danym napisie, co może być bardzo przydatne przy tworzeniu aplikacji lub gier. W tym artykule dowiesz się jak to zrobić oraz dlaczego warto.

## Jak to zrobić

Znajdowanie długości napisu w języku Java jest bardzo proste i wymaga tylko kilku linijek kodu. W tej sekcji pokażę Ci przykładowe rozwiązania wraz z wynikami.

```Java
// Zadeklarowanie napisu
String napis = "Cześć, tutaj piszę pierwszy blog post!";

// Użycie metody length() na obiekcie String
System.out.println("Długość napisu wynosi: " + napis.length());

// Wynik: Długość napisu wynosi: 37
```

Jak widać, używając metody `length()` na obiekcie typu `String`, otrzymujemy liczbę będącą długością tego napisu. Pamiętaj również, że liczymy każdy znak w tym napisie, włączając w to spacje i znaki specjalne.

Możemy również wykorzystać pętle, aby policzyć długość napisu i wyświetlić wynik dla każdego znaku oddzielnie.

```Java
// Zadeklarowanie napisu
String napis = "Hello there!";

// Użycie pętli for do policzenia długości
for (int i = 0; i < napis.length(); i++) {
    System.out.println("Długość znaku " + (i+1) + " to " + napis.charAt(i));
}

// Wynik:
// Długość znaku 1 to H
// Długość znaku 2 to e
// Długość znaku 3 to l
// Długość znaku 4 to l
// Długość znaku 5 to o
// Długość znaku 6 to
// Długość znaku 7 to t
// Długość znaku 8 to h
// Długość znaku 9 to e
// Długość znaku 10 to r
// Długość znaku 11 to e
// Długość znaku 12 to !
```

## Deep Dive

W przypadku większych napisów, często przydatne jest określenie długości tylko fragmentu tego napisu. W tym celu możemy użyć metody `substring()`.

```Java
// Zadeklarowanie napisu
String napis = "Lorem ipsum dolor sit amet";

// Użycie metody substring()
System.out.println("Fragment napisu: " + napis.substring(0, 5));

// Wynik: Fragment napisu: Lorem
```

W powyższym przykładzie, jako pierwszy argument metody `substring()` podajemy indeks, od którego chcemy zacząć, a jako drugi argument indeks końcowy (nie włączając tego indeksu). W ten sposób możemy uzyskać dowolny fragment napisu, który nas interesuje.

## Zobacz również

- Dokumentacja Oracle: [String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- Poradnik Java: [Jak znaleźć długość napisu w Javie](https://jakaprogramowac.pl/jak-znalezc-dlugosc-napisu-w-javie/)