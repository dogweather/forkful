---
title:                "Praca z plikami CSV"
html_title:           "Kotlin: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Dokąd idą programiści, gdy potrzebują przechowywać i przetwarzać duże ilości danych? Do CSV! CSV, czyli Comma Separated Values, jest formatem plików, który pozwala na przechowywanie danych w postaci tabeli, w której wartości są oddzielane przecinkami. Programiści wybierają CSV ze względu na jego prostotę i łatwość przetwarzania w programach.

## Jak to zrobić:

```Kotlin
val csvData = "Imię,Nazwisko,Wiek\n
               Anna,Kowalska,35\n
               Jan,Nowak,42\n
               Marta,Wójcik,27"
               
val lines = csvData.lines()
val headers = lines.first().split(",")
val people = lines.drop(1).map { it.split(",") }
    .map { Person(it[0], it[1], it[2].toInt()) }

data class Person(val firstName: String, val lastName: String, val age: Int)

// Przykładowe użycie:
for (person in people) {
    println("${person.firstName} ${person.lastName} ma ${person.age} lat.")
}

// Wyjście:
Anna Kowalska ma 35 lat.
Jan Nowak ma 42 lat.
Marta Wójcik ma 27 lat.
```

## Głębsze nurkowanie:

CSV jest formatem plików używanym od lat 70., a jego popularność ciągle rośnie ze względu na jego prostotę i powszechne wykorzystanie w różnych programach. Alternatywami dla CSV są między innymi JSON i XML, jednak CSV wciąż pozostaje jednym z najczęściej wybieranych formatów do przechowywania i przesyłania danych. Implementacja CSV w języku Kotlin jest prosta i wygodna dzięki wielu narzędziom dostępnym w języku, takim jak biblioteka oparta o standardową Java API lub biblioteki oparte o narzędzie Apache Commons CSV.

## Zobacz także:

- Dokumentacja biblioteki Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- Dokumentacja Java API dotycząca obsługi CSV: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html#lines--
- Reszta jest up to you – tak właśnie działa HTML!