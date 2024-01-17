---
title:                "Procurando e substituindo textos"
html_title:           "Kotlin: Procurando e substituindo textos"
simple_title:         "Procurando e substituindo textos"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por que? 
Substituir e pesquisar texto são tarefas comuns em programação. Substituir texto significa trocar uma sequência de caracteres por outra, enquanto que pesquisar texto significa procurar por uma determinada sequência dentro de um texto maior. Programadores muitas vezes precisam fazer isso para corrigir erros, alterar valores ou manipular dados de forma eficiente.

## Como fazer:
```Kotlin
// Exemplo de substituição
val texto = "Olá, mundo!"
val textoModificado = texto.replace("mundo", "universo")
println(textoModificado) // Output: Olá, universo!

// Exemplo de pesquisa
val texto = "Aprender Kotlin é divertido!"
val resultado = texto.contains("Kotlin")
println(resultado) // Output: true
```

## Deep Dive:
Substituir e pesquisar texto são técnicas que datam desde o início do desenvolvimento de linguagens de programação. Antigamente, programadores precisavam fazer isso manualmente, procurando e substituindo cada ocorrência de texto dentro de um código. Hoje em dia, existem muitas ferramentas e recursos que facilitam essa tarefa, como editores de código com recursos de busca e substituição integrados. Além disso, também é possível usar expressões regulares para tornar a pesquisa e substituição mais flexíveis e abrangentes.

## Veja também:
- [Documentação oficial do Kotlin para String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Tutorial sobre Expressões Regulares em Kotlin](https://www.raywenderlich.com/2975438-regular-expressions-in-kotlin#toc-anchor-001)
- [Vídeo explicativo sobre busca e substituição em Kotlin](https://www.youtube.com/watch?v=J5b9T722E7U)