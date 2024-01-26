---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W pracy z JSON chodzi o manipulowanie i przechowywanie danych w formacie, który jest prosty dla człowieka do odczytania, a dla maszyny do parsowania. Programiści używają JSON, bo jest elastyczny, współgra z większością technologii i jest łatwy w użyciu.

## Jak to zrobić:
Kotlin ułatwia pracę z JSON poprzez zintegrowane biblioteki. Oto przykład użycia biblioteki kotlinx.serialization do serializacji i deserializacji JSON:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json { prettyPrint = true }
    val data = User("Jan", 25)

    // Serializacja: Obiekt -> JSON
    val jsonString = json.encodeToString(User.serializer(), data)
    println(jsonString)

    // Deserializacja: JSON -> Obiekt
    val userData = json.decodeFromString(User.serializer(), jsonString)
    println(userData)
}
```

Output będzie wyglądał następująco:
```
{
    "name": "Jan",
    "age": 25
}
User(name=Jan, age=25)
```

## Wgłębiamy się
JSON (JavaScript Object Notation) był pierwotnie wiązany z JavaScriptem, ale stał się niezależnym standardem. Alternatywy to XML czy YAML, ale JSON jest popularniejszy z powodu swojej prostoty. Implementując obsługę JSON w Kotlinie, warto używać bibliotek takich jak kotlinx.serialization lub Moshi, które automatyzują większość procesu.

## Zobacz także
- Dokumentacja Kotlinx.serialization: [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- Oficjalna dokumentacja JSON: [JSON.org](https://www.json.org/json-en.html)
- Porównanie formatów danych (JSON vs XML vs YAML): [BitDegree Tutorials](https://www.bitdegree.org/learn/json-vs-xml-vs-yaml)
