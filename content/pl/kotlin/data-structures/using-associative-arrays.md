---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:05.976217-07:00
description: "Jak: Tworzenie i u\u017Cywanie mapy w Kotlinie jest proste. Oto kr\xF3\
  tki przewodnik, jak to zrobi\u0107."
lastmod: '2024-03-13T22:44:35.357764-06:00'
model: gpt-4-0125-preview
summary: "Tworzenie i u\u017Cywanie mapy w Kotlinie jest proste."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak:
Tworzenie i używanie mapy w Kotlinie jest proste. Oto krótki przewodnik, jak to zrobić:

```Kotlin
fun main() {
    // Tworzenie zmiennej mapy
    val owoce = mutableMapOf("a" to "Jabłko", "b" to "Banan")

    // Dodawanie elementów
    owoce["o"] = "Pomarańcza" // Używając operacji indeksowania
    owoce.put("g", "Winogrono") // Używając metody put

    // Dostęp do elementów
    println(owoce["a"])  // Wyjście: Jabłko
    println(owoce["b"])  // Wyjście: Banan

    // Usuwanie elementów
    owoce.remove("b")
    
    // Iteracja po mapie
    for ((klucz, wartosc) in owoce) {
        println("$klucz -> $wartosc")
    }
    // Przykładowe wyjście:
    // a -> Jabłko
    // o -> Pomarańcza
    // g -> Winogrono
}
```

## Szczegółowa analiza
Mapy Kotlinowe pochodzą bezpośrednio z jego interoperacyjności z Javą, gdzie mapy są istotną częścią kolekcji. Jednak Kotlin zwiększa ich użyteczność, dostarczając zarówno zmienne (`MutableMap`), jak i tylko do odczytu (`Map`) interfejsy, w przeciwieństwie do unifikowanego interfejsu `Map` Javy. To rozróżnienie jasno określa, czy kolekcja jest przeznaczona do modyfikacji, czy nie.

Istotnym szczegółem w implementacji map w Kotlinie jest wyraźne rozróżnienie między mapami mutowalnymi a niemutowalnymi, co podkreśla skupienie języka na niemutowalności i bezpieczeństwie wątków.

Choć mapy są bardzo użyteczne, Kotlin oferuje również inne kolekcje, takie jak listy i zbiory, z których każda ma własne przypadki użycia. Na przykład listy zachowują kolejność i pozwalają na duplikaty, co czyni je idealnymi do dostępu do elementów po indeksie, podczas gdy zbiory zapewniają unikalność, ale nie zachowują kolejności. Wybór między użyciem mapy, listy czy zbioru zależy od konkretnych wymagań aplikacji, takich jak potrzeba dostępu opartego na kluczach czy zachowanie kolejności.

Jeśli chodzi o lepsze alternatywy, w przypadku gdy wydajność jest kluczowa, zwłaszcza przy dużych kolekcjach, rozważ użycie specjalistycznych, bardziej wydajnych struktur danych dostarczanych przez zewnętrzne biblioteki, które są zoptymalizowane pod kątem konkretnych przypadków użycia, takich jak dostęp współbieżny czy sortowanie.
