---
title:    "Java: Konwertowanie ciągu znaków na małe litery"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie ciągu znaków na małe litery jest powszechnie używaną funkcją w języku Java. Jest to szczególnie przydatne w przypadku użytkowników, którzy chcą wprowadzić dane do programu w formacie, który jest niezależny od wielkości liter. Zmiana wielkości liter może również ułatwić porównywanie danych.

## Jak to zrobić

```Java
public static void main(String[] args) {

    // Przykładowy ciąg znaków
    String name = "Java Programming";

    // Użycie metody toLowerCase() do konwertowania na małe litery
    String lowerCaseName = name.toLowerCase();

    // Wyświetlenie wyniku
    System.out.println("Przed konwersją: " + name);
    System.out.println("Po konwersji: " + lowerCaseName);
}
```

Output: 
```
Przed konwersją: Java Programming
Po konwersji: java programming
```

## Dogłębne zanurzenie

Konwersja ciągu znaków na małe litery odbywa się za pomocą metody `toLowerCase()`, która jest dostępna dla każdego obiektu typu `String`. W języku Java, wszystkie ciągi znaków są traktowane jako obiekty i dlatego mogą mieć różne metody dostępne do użycia. Metoda `toLowerCase()` przekształca wszystkie znaki na małe litery i zwraca wynik jako nowy obiekt `String`.

Jedną z głównych przyczyn, dla których konwertujemy ciągi znaków na małe litery jest uniknięcie problemów związanych z wielkością liter w danych wejściowych. Dzięki temu, że wszystkie znaki są konwertowane na małe, nie będzie znaczenia, czy użytkownik wprowadził dane z małej czy wielkiej litery.

## Zobacz również

- [Java String Methods](https://www.w3schools.com/java/java_ref_string.asp)
- [Konwertowanie liczb na tekst w języku Java](https://javatutorial.net/java-convert-number-to-string)
- [Official Java Documentation on String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)