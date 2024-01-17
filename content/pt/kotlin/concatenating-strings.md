---
title:                "Inserindo Strings"
html_title:           "Kotlin: Inserindo Strings"
simple_title:         "Inserindo Strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que?

Concatenar strings é a ação de juntar duas ou mais cadeias de caracteres em uma única cadeia de caracteres. Isso é uma tarefa comum para muitos programadores, pois permite criar mensagens personalizadas ou manipular dados de maneira eficiente.

## Como fazer:

Veja abaixo dois exemplos simples de como concatenar strings em Kotlin:

```Kotlin
val nome = "Maria"
val sobrenome = "Silva"
println("Olá " + nome + " " + sobrenome) // Saída: "Olá Maria Silva"
```

```Kotlin
val idade = 30
val mensagem = "Eu tenho " + idade + " anos!"
println(mensagem) // Saída: "Eu tenho 30 anos!"
```

## Profundando:

Historicamente, a concatenação de strings era feita através do operador `+` em muitas linguagens de programação, incluindo Kotlin. No entanto, essa abordagem pode ser ineficiente, pois a cada concatenação uma nova string é criada na memória, o que pode ser problemático em loops e operações repetitivas.

Uma alternativa mais eficiente é usar a classe `StringBuilder` em Kotlin. Esta classe permite concatenar strings sem criar novas instâncias na memória, o que resulta em melhor desempenho. Um exemplo de como utilizá-la seria:

```Kotlin
val sb = StringBuilder()
sb.append("Eu sou")
sb.append(" um programa")
sb.append(" de concatenação.")
val resultado = sb.toString()
println(resultado) // Saída: "Eu sou um programa de concatenação."
```

## Veja também:

Para saber mais sobre strings em Kotlin:
- [Documentação oficial do Kotlin sobre strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial de Strings em Kotlin](https://www.baeldung.com/kotlin/strings)
- [Exemplos de concatenação de strings em Kotlin](https://www.programiz.com/kotlin-programming/string-concatenation)