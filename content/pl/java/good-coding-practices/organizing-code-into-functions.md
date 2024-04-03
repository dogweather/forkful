---
date: 2024-01-26 01:10:53.222145-07:00
description: "Jak to zrobi\u0107: Oto klasyczny przyk\u0142ad \u2014 funkcja do obliczania\
  \ silni liczby."
lastmod: '2024-03-13T22:44:35.282767-06:00'
model: gpt-4-1106-preview
summary: "Oto klasyczny przyk\u0142ad \u2014 funkcja do obliczania silni liczby."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Oto klasyczny przykład — funkcja do obliczania silni liczby.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Silnia liczby " + number + " wynosi: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Wynik będzie następujący:
```
Silnia liczby 5 wynosi: 120
```

## Szczegółowa analiza
Zanim funkcje stały się powszechne, kod był upychany w monolityczne bloki, przez co debugowanie przypominało szukanie igły w stogu siana. Teraz, enkapsulacja funkcjonalności w funkcje pomaga szybko izolować problemy. Alternatywami są wyrażenia lambda w Javie lub metody w programowaniu obiektowym, oba pełniące podobne funkcje. Pamiętaj, pisząc funkcję: (1) Każda funkcja powinna mieć pojedynczą odpowiedzialność oraz (2) nazwa funkcji powinna jasno opisywać jej cel.

## Zobacz także
Aby dowiedzieć się więcej o organizacji kodu:
- "Czysty kod. Podręcznik dobrego programisty" autorstwa Roberta C. Martina
- "Refaktoryzacja. Udoskonalanie struktury istniejącego kodu" autorstwa Martina Fowlera
- [Dokumentacja Java Oracle na temat definiowania metod](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
