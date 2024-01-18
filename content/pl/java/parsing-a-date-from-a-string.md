---
title:                "Analiza daty z ciągu znaków"
html_title:           "Java: Analiza daty z ciągu znaków"
simple_title:         "Analiza daty z ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Konwersja daty z ciągu znaków to proces zamieniający datę w formacie tekstowym na obiekt daty w języku Java. Programiści często wykorzystują ten proces do ułatwienia manipulacji i porównywania dat w swoich programach.

## Jak to zrobić:

```Java 
String date = "15/05/2021"; // przykładowy ciąg znaków z datą
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy"); // tworzymy format daty
LocalDate parsedDate = LocalDate.parse(date, formatter); // konwertujemy datę na obiekt daty
System.out.println(parsedDate); // wypisujemy wynik: 2021-05-15
```

## Głębszy wgląd:

Historia konwersji daty z ciągu znaków sięga czasów przed powstaniem języka Java. Wcześniej programiści musieli samodzielnie tworzyć funkcje do obróbki dat, co często powodowało błędy i problemy z obsługą różnych formatów dat.

Obecnie istnieje wiele alternatywnych bibliotek i narzędzi do konwersji daty w języku Java. Jednak wbudowane klasy i funkcje dostępne w języku są wystarczające dla większości potrzeb.

Podczas konwersji daty z ciągu znaków należy uważać na różnice w formatach dat pomiędzy różnymi regionami i ustawieniami systemowymi, co może powodować nieoczekiwane wyniki.

## Zobacz także:

- [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) - klasa do parsowania i formatowania dat w języku Java.
- [Java 8 Date Time API](https://www.baeldung.com/java-8-date-time-intro) - wprowadzenie do nowego API obsługującego daty i czasy w języku Java od wersji 8.