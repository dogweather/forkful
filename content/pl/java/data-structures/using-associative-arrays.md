---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:52.664013-07:00
description: "Jak to zrobi\u0107: Java nie posiada wbudowanych tablic asocjacyjnych,\
  \ jak niekt\xF3re j\u0119zyki, ale udost\u0119pnia interfejs `Map` oraz klasy takie\
  \ jak `HashMap` i\u2026"
lastmod: '2024-03-13T22:44:35.270253-06:00'
model: gpt-4-0125-preview
summary: "Java nie posiada wbudowanych tablic asocjacyjnych, jak niekt\xF3re j\u0119\
  zyki, ale udost\u0119pnia interfejs `Map` oraz klasy takie jak `HashMap` i `TreeMap`,\
  \ by spe\u0142ni\u0107 t\u0119 rol\u0119."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Java nie posiada wbudowanych tablic asocjacyjnych, jak niektóre języki, ale udostępnia interfejs `Map` oraz klasy takie jak `HashMap` i `TreeMap`, by spełnić tę rolę. Oto jak użyć `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Tworzenie HashMap
        Map<String, Integer> wiekPrzyjaciol = new HashMap<>();
        
        // Dodawanie elementów
        wiekPrzyjaciol.put("Alice", 24);
        wiekPrzyjaciol.put("Bob", 30);
        wiekPrzyjaciol.put("Charlie", 28);

        // Dostęp do elementów
        System.out.println("Wiek Alice: " + wiekPrzyjaciol.get("Alice"));
        
        // Obsługa kluczy nieistniejących
        System.out.println("Wiek osoby nieobecnej na mapie: " + wiekPrzyjaciol.getOrDefault("Dan", -1));

        // Iteracja po elementach
        for (Map.Entry<String, Integer> wpis : wiekPrzyjaciol.entrySet()) {
            System.out.println(wpis.getKey() + " ma " + wpis.getValue() + " lat(a).");
        }
    }
}
```

Przykładowe wyjście:

```
Wiek Alice: 24
Wiek osoby nieobecnej na mapie: -1
Alice ma 24 lata.
Bob ma 30 lat.
Charlie ma 28 lat.
```

`HashMap` to tylko jedna z implementacji. Jeśli twoje klucze są unikalne i potrzebujesz ich posortowania, rozważ `TreeMap`. Dla mapy, która zachowuje kolejność wstawiania, `LinkedHashMap` jest Twoim przyjacielem.

## Głębsze zanurzenie
Mapy w Javie są częścią Frameworka Kolekcji, wprowadzonego w JDK 1.2, ale z czasem doczekały się znaczących ulepszeń, w tym wprowadzenia metody `forEach` w Java 8 dla łatwiejszej iteracji po wpisach. Wybór implementacji mapy (`HashMap`, `LinkedHashMap`, `TreeMap`) powinien być dyktowany konkretnymi potrzebami w zakresie porządkowania i wydajności. Na przykład, `HashMap` oferuje wydajność czasową O(1) dla podstawowych operacji (get i put), zakładając, że funkcja hash rozdziela elementy odpowiednio pomiędzy kubełki. Jednakże, jeśli potrzebujesz sortowania na podstawie naturalnego porządkowania lub niestandardowych komparatorów, `TreeMap` jest najlepszym wyborem, oferując O(log n) czasu dla wstawiania i wyszukiwania.

Przed wprowadzeniem `Map`, tablice asocjacyjne były zazwyczaj implementowane za pomocą dwóch równoległych tablic (jednej dla kluczy, drugiej dla wartości) lub niestandardowych struktur danych o mniejszej wydajności. Obecne alternatywy dla `Map` i jej implementacji mogą obejmować biblioteki stron trzecich oferujące specjalizowane mapy, takie jak mapy dwukierunkowe (BiMap w bibliotece Google's Guava) na wypadek, gdy potrzebujesz efektywnie znaleźć klucz po jego wartości. Jednakże, dla większości przypadków użycia w Javie, mapy biblioteki standardowej są wystarczająco solidne i elastyczne, aby sprostać zadaniu.
