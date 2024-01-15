---
title:                "Praca z yaml"
html_title:           "Kotlin: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli zajmujesz się programowaniem w języku Kotlin i potrzebujesz wygodnego formatu do przechowywania i przesyłania danych, YAML może być idealnym wyborem. Ten format jest czytelny dla ludzi, łatwy do wprowadzania zmian i wspiera szeroki zakres typów danych. Czytaj dalej, aby dowiedzieć się, dlaczego warto zapoznać się z YAML w kontekście programowania Kotlin.

## Jak to zrobić

Kotlin dostarcza nam wygodną bibliotekę o nazwie "kotlinx.serialization", która umożliwia nam pracę z YAML. Najpierw musimy zdefiniować model danych (klasę) do serializacji w formacie YAML. Następnie możemy wywołać funkcję "encode" i przekazać jej nasz model danych, aby otrzymać reprezentację YAML w postaci tekstu. Przykładowy kod wygląda następująco:

```Kotlin
// Definicja modelu danych
@Serializable
data class User(val name: String, val age: Int)

val user = User("John", 25)

// Serializacja do formatu YAML
val yaml = Yaml.default.encodeToString(User.serializer(), user)
println(yaml)
```

W powyższym kodzie tworzymy prosty model danych "User", który jest oznaczony adnotacją "@Serializable", aby mogła być wykorzystana przez bibliotekę serialization. Następnie wywołujemy funkcję "encodeToString", przekazując nasz model danych oraz jego serializator. Otrzymana reprezentacja YAML zostaje wyświetlona na ekranie.

Możemy również zmodyfikować pewne aspekty serializacji, na przykład wybrać wybrane pola do uwzględnienia lub wykluczenia, korzystając z adnotacji "@SerialName" oraz "@Transient". Pełną dokumentację biblioteki można znaleźć na oficjalnej stronie.

## Deep Dive

Biblioteka "kotlinx.serialization" oferuje także możliwość deserializacji danych z formatu YAML na obiekty Kotlin. Po prostu wywołujemy funkcję "decodeFromString" i podajemy jej odpowiedni serializator oraz tekst w formacie YAML, a otrzymane dane zostaną przypisane do instancji naszego modelu danych. Przykładowy kod wygląda następująco:

```Kotlin
// Deserializacja z formatu YAML do modelu danych
val yaml = """
    name: Jane
    age: 30
""".trimIndent()

val user = Yaml.default.decodeFromString(User.serializer(), yaml)
println(user)
```

W powyższym kodzie tworzymy tekst w formacie YAML i przekazujemy go do funkcji "decodeFromString", w której podajemy odpowiedni serializator. Otrzymujemy instancję naszego modelu danych, którą wyświetlamy na ekranie.

Biblioteka ta obsługuje także różne typy kolekcji, walidację danych oraz dostarcza zaawansowanych funkcji, na przykład możliwość zmieniania nazw pól w trakcie serializacji lub deserializacji. Dzięki temu możemy dopasować format YAML do swoich potrzeb.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat YAML i jak go wykorzystać w języku Kotlin, polecam zapoznanie się z poniższymi linkami:

- Oficjalna dokumentacja biblioteki "kotlinx.serialzation": https://github.com/Kotlin/kotlinx.serialization
- Przykłady kodu i użycia biblioteki: https://github.com/Kotlin/kotlinx.serialization/tree/master/formats/yaml/yaml-tests
- Poradnik na Medium o wykorzystaniu YAML w Kotlinie: https://medium.com/@pawelgorniak/yaml-serialization-in-kotlin-b9e5a7018eb