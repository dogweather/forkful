---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odnalezienie długości napisu (string) to proces liczenia znaków w konkretnym stringu. Programiści robią to do walidacji danych wejściowych, operacji na stringach i wiele innych.

## Jak to zrobić:

Złotym standardem dla znalezienia długości stringu w Java jest metoda `length()`. Oto przykład:

```Java
public class Main {
  public static void main(String []args) {
    String example = "Programowanie w Javie";
    int stringLength = example.length();
    System.out.println("Długość napisu: " + stringLength);
  }
}
```

Wyświetlany wynik:

```
Długość napisu: 22
```

## Głębsze zrozumienie

(1) Historia: Koncepcja znalezienia długości ciągu pochodzi z czasów, kiedy programiści musieli bezpośrednio zarządzać pamięcią. 

(2) Alternatywy: Inna metoda to używanie `toCharArray()` do konwersji stringu na tablicę znaków, a następnie wywołanie `length`. Ale to jest mniej wydajne.

```Java
public class Main {
    public static void main(String[] args) {
        String example = "Programowanie w Javie";
        int length = example.toCharArray().length;
        System.out.println("Długość napisu: " + length);
    }
}
```

(3) Szczegóły implementacji: W Javie `String.length()` jest napisane w języku C i jest bardzo efektywne. Korzysta bezpośrednio z informacji przechowywanych w instancji klasy String.

## Przydatne linki

Java String length() Method - https://www.javatpoint.com/java-string-length

Java String Class Documentation - https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html