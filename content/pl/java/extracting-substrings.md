---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:46:12.958077-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Wycinanie podciągów to pobieranie fragmentów z większego ciągu znaków. Programiści robią to, by manipulować danymi, wyciągać informacje lub sprawdzać format.

## How to: (Jak to zrobić?)

Java używa metody `substring()` klasy `String` do wycinania podciągów. Przykłady:

```java
public class SubstringExample {

    public static void main(String[] args) {
        String fullString = "Witaj, programisto!";
        String extracted = fullString.substring(7, 19);
        
        System.out.println(extracted); // Wypisze: programisto
    }
}
```

Output:
```
programisto
```

Możemy też wyciąć od konkretnej pozycji do końca:

```java
public class SubstringExampleEnd {

    public static void main(String[] args) {
        String fullString = "Dobra praktyka programistyczna";
        String extracted = fullString.substring(13);
        
        System.out.println(extracted); // Wypisze: programistyczna
    }
}
```

Output:
```
programistyczna
```

## Deep Dive (Głębsze spojrzenie)

Metoda `substring()` istnieje w Javie od jej wczesnych wersji. W starszych wersjach Javy (przed Java 7 Update 6), używanie `substring()` w niektórych przypadkach mogło prowadzić do niespodziewanych problemów z wydajnością i pamięcią, bo wewnętrznie wyciągnięte podciągi wskazywały na te same tablice znaków co oryginalny ciąg.

Alternatywne metody obejmują użycie `String.split()`, klas `StringBuilder` czy `StringTokenizer`. Każda ma swoje zastosowania w zależności od konkretnego scenariusza.

Szczegół implementacyjny: od Java 7 Update 6, `substring()` tworzy nowy ciąg znaków, co oznacza, że nie dzieli już pamięci z oryginalnym ciągiem, co jest bezpieczniejsze dla zarządzania pamięcią.

## See Also (Zobacz też)

- Dokumentacja Oracle dla klasy [String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- Java API specyfikacja dla metody [substring](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int))
- Przewodnik Oracle do dobrych praktyk [Java Performance](https://docs.oracle.com/javase/8/docs/technotes/guides/performance/index.html)