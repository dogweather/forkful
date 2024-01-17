---
title:                "Trabalhando com json"
html_title:           "Kotlin: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Trabalhar com JSON é manipular dados em um formato muito popular e fácil de ler e escrever. Programadores usam JSON para armazenar e transmitir informações entre aplicações, especialmente em aplicações web que se conectam a APIs. 

## Como fazer:

```Kotlin
// Ler um arquivo JSON e converter para um objeto Kotlin
val jsonString = File("exemplo.json").readText()
val objeto = Klaxon().parse<MeuObjeto>(jsonString)

// Converter um objeto Kotlin para JSON
val meuObjeto = MeuObjeto("exemplo", 123)
val jsonString = Klaxon().toJsonString(meuObjeto)
```

## Profundidade:

JSON, ou JavaScript Object Notation, foi criado em 1999 como uma alternativa leve e fácil de ler para XML. Atualmente, é amplamente utilizado na web e em APIs por sua simplicidade e compatibilidade com múltiplas linguagens de programação. Além do Kotlin, existem outras bibliotecas que podem ser utilizadas para manipulação de JSON, como o Gson e o Jackson.

## Veja também:

https://www.json.org/json-pt.html
https://kotlinlang.org/api/latest/jvm/stdlib/kotlinx.coroutines/-json-string/index.html
https://github.com/cbeust/klaxon