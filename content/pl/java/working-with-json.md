---
title:                "Praca z json"
html_title:           "Java: Praca z json"
simple_title:         "Praca z json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Programowanie z JSON- em to proces przetwarzania i przechowywania danych w formacie JSON (JavaScript Object Notation). Jest to popularny sposób na wymianę danych między aplikacjami i serwerami. Programiści używają JSON-a, ponieważ jest on prosty, przenośny i łatwy do zrozumienia.

## Jak to zrobić:

```java
// Tworzenie obiektu JSON 
JSONObject obj = new JSONObject();

// Dodawanie danych do obiektu
obj.put("imie", "Anna");
obj.put("wiek", 30);
obj.put("hobby", "piesze wędrówki");

// Konwertowanie obiektu na string i wyświetlenie
String json = obj.toString();
System.out.println(json);

// Wynik: {"imie":"Anna","wiek":30,"hobby":"piesze wędrówki"}
```

## Wprowadzenie w głąb:
JSON został stworzony w 2001 roku i jest powszechnie używany w dzisiejszych czasach. Istnieją również inne formaty danych, takie jak XML czy CSV, ale JSON jest popularny ze względu na swoją prostotę i elastyczność. W Java, programiści mogą używać różnych bibliotek, takich jak `org.json` lub `GSON`, aby pracować z JSON-em.

## Zobacz też:
- [Oficjalna dokumentacja JSON](https://www.json.org/json-pl.html)
- [Kurs JSON w Javie na YouTube](https://www.youtube.com/watch?v=071UmQJ1F-U)
- [Porównanie JSON-a z innymi formatami danych](https://www.geeksforgeeks.org/json-vs-xml/)