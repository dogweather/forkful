---
title:                "Kotlin: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como funciona a junção de duas ou mais strings em um programa? A concatenação de strings é uma operação essencial em muitos programas, permitindo que você una várias partes de texto para formar uma única string. Neste artigo, vamos explorar como realizar essa operação em Kotlin e por que ela é importante em suas criações de software.

## Como Fazer

Em Kotlin, a concatenação de strings é feita usando o operador "+". Vejamos um exemplo simples:

```Kotlin
val nome = "João"
val sobrenome = "Silva"
val nomeCompleto = nome + sobrenome
println(nomeCompleto)

// Output: João Silva
```

No exemplo acima, criamos três variáveis: "nome", "sobrenome" e "nomeCompleto". A variável "nomeCompleto" é a junção das duas primeiras variáveis usando o operador "+". A seguir, usamos a função "println()" para imprimir o valor dessa variável, e o resultado é "João Silva".

Também é possível fazer a concatenação de strings com variáveis de outros tipos de dado, como números. Porém, nesse caso, é necessário converter os valores para string antes de realizar a operação. Vejamos um exemplo:

```Kotlin
val idade = 25
val mensagem = "Eu tenho " + idade.toString() + " anos"
println(mensagem)

// Output: Eu tenho 25 anos
```

## Mergulho Profundo

A concatenação de strings pode ser uma tarefa simples, mas é importante entender como ela funciona em nível mais profundo. Quando usamos o operador "+", o Kotlin cria um novo objeto da classe "String" contendo os valores das duas variáveis. Isso significa que a concatenação de strings pode levar a um aumento no uso de memória, principalmente quando usada repetidamente em um loop.

Uma maneira de otimizar esse processo é usar a classe "StringBuilder". Essa classe possui um método "append()" que permite adicionar strings sem precisar criar um novo objeto a cada operação. Vejamos o mesmo exemplo anterior, mas agora utilizando o StringBuilder:

```Kotlin
val nome = "João"
val sobrenome = "Silva"
val nomeCompleto = StringBuilder(nome).append(sobrenome).toString()
println(nomeCompleto)

// Output: João Silva
```

Dessa forma, evitamos a criação de objetos desnecessários e melhoramos o desempenho da concatenação de strings.

Agora que você entende como fazer a concatenação de strings em Kotlin e sabe um pouco mais sobre o funcionamento dessa operação, pode usar esse recurso em seus projetos de forma mais eficiente.

## Veja Também

- [Documentação oficial do Kotlin sobre a concatenação de strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Exemplos de concatenação de strings em Kotlin](https://www.programiz.com/kotlin-programming/concatenate-string)
- [Mais informações sobre a classe StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)