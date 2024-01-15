---
title:                "Lavorare con json"
html_title:           "Kotlin: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Perché
In un'era in cui i dati sono il fulcro di molte applicazioni, lavorare con JSON è diventato fondamentale. JSON (JavaScript Object Notation) è un formato leggero, flessibile e facilmente leggibile dai computer e dagli esseri umani, rendendolo uno dei formati più popolari per lo scambio di dati.

## Come Fare
Per iniziare a lavorare con JSON in Kotlin, è necessario importare la libreria "kotlinx-serialization-json" e creare una classe di dati che rappresenti la struttura del nostro JSON.
```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    // Serializza un oggetto User
    val user = User("John", 30)
    val json = Json.encodeToString(user)
    println(json) // Output: {"name": "John", "age": 30}
    
    // Deserializza una stringa JSON in un oggetto User
    val jsonString = """{"name": "Jane", "age": 25}"""
    val parsedUser = Json.decodeFromString<User>(jsonString)
    println(parsedUser.name) // Output: Jane
}
```
È anche possibile lavorare con oggetti JSON annidati, come nel seguente esempio:
```Kotlin
@Serializable
data class Book(val title: String, val author: String)

@Serializable
data class Library(val name: String, val books: List<Book>)

fun main() {
    // Serializza un oggetto Library
    val library = Library("Biblioteca Nazionale", listOf(Book("Opere Complete", "William Shakespeare"), Book("I Malavoglia", "Giovanni Verga")))
    val json = Json.encodeToString(library)
    println(json) // Output: {"name": "Biblioteca Nazionale", "books": [{"title": "Opere Complete", "author": "William Shakespeare"}, {"title": "I Malavoglia", "author": "Giovanni Verga"}]}
    
    // Deserializza una stringa JSON in un oggetto Library
    val jsonString = """{"name": "Biblioteca Nazionale", "books": [{"title": "Il Processo", "author": "Franz Kafka"}, {"title": "1984", "author": "George Orwell"}]}"""
    val parsedLibrary = Json.decodeFromString<Library>(jsonString)
    println(parsedLibrary.books[1].title) // Output: 1984
}

```

## Approfondimento
Kotlin fornisce molte funzioni utili per lavorare con JSON, come ad esempio la possibilità di definire nomi di proprietà personalizzati o di ignorare alcune proprietà durante la deserializzazione. È inoltre importante tenere conto del fatto che JSON può rappresentare solo un certo numero di tipi di dati, come stringhe, numeri, booleani, null, array e oggetti, quindi la conversione può comportare la perdita di alcune informazioni.

## Vedi Anche
- [Documentazione ufficiale di Kotlin per lavorare con JSON](https://kotlinlang.org/docs/serialization.html)
- [Libreria kotlinx-serialization-json](https://github.com/Kotlin/kotlinx.serialization)
- [Tutorial: Lavorare con JSON in Kotlin](https://www.raywenderlich.com/5485-kotlin-and-json-parsing)