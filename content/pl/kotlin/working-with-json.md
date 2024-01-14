---
title:                "Kotlin: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Przetwarzanie danych w formacie JSON jest niezbędne dla wielu dzisiejszych aplikacji internetowych. Właściwe zrozumienie tego formatu oraz umiejętność jego obsługi jest kluczowe dla każdego programisty.

## Jak to zrobić

```Kotlin
// Przykładowe dane w formacie JSON
val json = """
    {
        "name": "Jan Kowalski",
        "age": 30,
        "hobbies": ["jazda na rowerze", "gotowanie", "podróże"],
        "address": {
            "street": "ul. Kwiatowa 5",
            "city": "Warszawa",
            "country": "Polska"
        }
    }
""".trimIndent()

// Konwersja danych z formatu JSON na obiekt Kotlin
val person = Gson().fromJson(json, Person::class.java)

// Wyświetlenie nazwy oraz wieku osoby
println("Imię: ${person.name}")
println("Wiek: ${person.age}")

// Iteracja przez listę zainteresowań
println("Zainteresowania:")
person.hobbies.forEach { hobby ->
    println(hobby)
}

// Wyświetlenie pełnego adresu
println("Adres:")
println("${person.address.street}, ${person.address.city}, ${person.address.country}")
```
**Output:**

Imię: Jan Kowalski
Wiek: 30
Zainteresowania:
- jazda na rowerze
- gotowanie
- podróże
Adres:
ul. Kwiatowa 5, Warszawa, Polska

## Deep Dive

JSON (JavaScript Object Notation) jest lekkim i czytelnym formatem danych, który jest powszechnie wykorzystywany w aplikacjach internetowych. W Kotlinie istnieje wiele narzędzi, takich jak biblioteka Gson, która ułatwia manipulowanie danymi w tym formacie. W powyższym przykładzie wykorzystujemy Gson do deserializacji danych z formatu JSON na obiekt Kotlin oraz do wyświetlenia ich w czytelny sposób.

Dodatkowo, warto zwrócić uwagę na hierarchiczną strukturę danych w JSONie, co pozwala na przechowywanie dużych ilości informacji w przystępny sposób. W przypadku bardziej zaawansowanych operacji, warto zapoznać się z dokumentacją biblioteki Gson oraz dostępnych funkcji w Kotlinie.

## Zobacz również

- Dokumentacja biblioteki Gson: https://github.com/google/gson
- Oficjalna strona języka Kotlin: https://kotlinlang.org/
- Jak działa format JSON? (artykuł w języku polskim): https://pl.wikipedia.org/wiki/JavaScript_Object_Notation