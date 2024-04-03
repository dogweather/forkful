---
date: 2024-01-20 17:46:12.958077-07:00
description: "How to: (Jak to zrobi\u0107?) Java u\u017Cywa metody `substring()` klasy\
  \ `String` do wycinania podci\u0105g\xF3w. Przyk\u0142ady."
lastmod: '2024-03-13T22:44:35.266100-06:00'
model: gpt-4-1106-preview
summary: "Java u\u017Cywa metody `substring()` klasy `String` do wycinania podci\u0105\
  g\xF3w."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

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
