---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przekształcanie daty na łańcuch znaków to proces zmiany typu danych reprezentujących datę na ciąg znaków, który może być łatwiej manipulowany lub wyświetlany. Programiści robią to, aby ułatwić wyświetlanie dat lub zapisywanie ich w plikach i bazach danych.

## Jak to zrobić:

Zobaczmy jak możemy przekształcić datę na łańcuch znaków w Javie:
```Java
import java.text.SimpleDateFormat; 
import java.util.Date;  

public class Main 
{ 
    public static void main(String[] args) 
    { 
        Date date = new Date(); 
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss"); 
        String strDate = formatter.format(date); 
        System.out.println("Data w formie łańcucha znaków: " + strDate); 
    } 
}
```
Wyjście:
```
Data w formie łańcucha znaków: 10-05-2022 14:55:30
```
Klasa `SimpleDateFormat` pozwala nam sformatować datę według określonego wzorca.

## Głębsza wiedza:

Historia: Przed wprowadzeniem klas Java 8 do obsługi dat i czasu, najczęściej używaną klasą do manipulacji datą była klasa `java.util.Date`.

Alternatywy: Od Java 8, obiekty daty można konwertować do łańcuchów znaków za pomocą klas `java.time.LocalDateTime` i `java.time.format.DateTimeFormatter`. 

Szczegóły implementacji: Java korzysta z domyślnej strefy czasowej i lokalizacji (języka, kraju i wariantu), które mogą wpływać na formatowanie daty. Można to dostosować za pomocą SimpleDateFormat.

## Zobacz także:

[Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
[Dokumentacja klas Java 8 LocalDateTime i DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
[Praca z datami w Javie](https://www.baeldung.com/java-8-date-time-intro)