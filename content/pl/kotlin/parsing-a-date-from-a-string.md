---
title:                "Parsowanie daty z ciągu znaków"
html_title:           "Kotlin: Parsowanie daty z ciągu znaków"
simple_title:         "Parsowanie daty z ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsing daty z ciągu znaków polega na otrzymaniu daty w wybranej formie, np. obiektu Date lub LocalDate, po przetworzeniu ciągu znaków. Programiści robią to, ponieważ często otrzymują dane w postaci tekstu, a niegotowe użycie tej danych jest potrzebne.

## Jak to zrobić:

### Przykład 1:
Parsing daty z ciągu znaków w postaci tekstu - "12/12/2020" i uzyskanie obiektu LocalDate:
```Kotlin
val sdf = SimpleDateFormat("dd/MM/yyyy")
val date = sdf.parse("12/12/2020")
println(date)
```
Output: ```Sat Dec 12 00:00:00 CET 2020```

### Przykład 2:
Parsing daty z ciągu znaków w postaci tekstu - "12/12/2020" i uzyskanie obiektu Date:
```Kotlin
val sdf = SimpleDateFormat("dd/MM/yyyy")
val date = sdf.parse("12/12/2020")
println(date)
```
Output: ```Sat Dec 12 00:00:00 CET 2020```

## Głębsze wgląd:

### Kontekst historyczny:
Pierwsze metody parsowania daty w językach programowania pojawiły się w latach 70. XX wieku, kiedy to potrzebne było przetwarzanie i analiza dużych ilości danych.

### Alternatywy:
W Kotlinie, obecnie najlepszą opcją jest korzystanie z klasy Date oraz klasy LocalDate z pakietu java.time, wprowadzonej w wersji 1.8 Javy. Wcześniej często używane były klasy Date i Calendar.

### Szczegóły implementacji:
Klasa SimpleDateFormat jest wykorzystywana do parsowania daty w postaci tekstu dzięki możliwości ustawienia wzorca daty. Następnie, metoda parse przetwarza ciąg znaków i zwraca obiekt Date lub LocalDate, w zależności od wybranego wzorca.

## Zobacz także:
- [Dokumentacja Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Dokumentacja Java LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Kotlin Date vs LocalDate](https://javarevisited.blogspot.com/2017/12/java-date-vs-java-time-kotlin-and-date.html)