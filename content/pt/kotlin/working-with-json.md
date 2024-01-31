---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
JSON (JavaScript Object Notation) é um formato leve para troca de dados. Programadores usam JSON porque ele é fácil de ler e escrever para humanos e é analisado (parsed) e gerado facilmente por máquinas.

## Como Fazer:
```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

// Definindo uma data class
@Serializable
data class Usuario(val nome: String, val idade: Int)

fun main() {
    // Serializando para JSON
    val usuario = Usuario("Carlos", 29)
    val json = Json.encodeToString(usuario)
    println(json) // Saída: {"nome":"Carlos","idade":29}

    // Deserializando do JSON
    val jsonString = """{"nome":"Ana","idade":34}"""
    val usuarioObj = Json.decodeFromString<Usuario>(jsonString)
    println(usuarioObj) // Saída: Usuario(nome=Ana, idade=34)
}
```

## Mergulho Profundo
O uso de JSON cresceu com a popularidade de APIs RESTful desde meados dos anos 2000, desbancando XML. Alternativas incluem YAML e Protocol Buffers, mas JSON mantém a liderança em uso por ser nativo em JavaScript e de fácil interação. Na implementação, Kotlin usa a biblioteca kotlinx.serialization para facilitar a conversão entre objetos Kotlin e JSON.

## Veja Também
- Documentação oficial de Kotlin Serialization: [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- Especificação JSON: [json.org](https://www.json.org/json-pt.html)
