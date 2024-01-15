---
title:                "Juntando strings"
html_title:           "Kotlin: Juntando strings"
simple_title:         "Juntando strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma técnica muito útil e comum na programação Kotlin. Ao unir diferentes strings, é possível criar mensagens personalizadas, gerar relatórios e formatar dados para exibição. Além disso, é uma habilidade importante para quem deseja se tornar um programador Kotlin completo.

## Como fazer

Para concatenar strings em Kotlin, podemos utilizar o operador "+". Veja o exemplo abaixo:

```Kotlin
val nome = "João"
val sobrenome = "Silva"
val nomeCompleto = nome + sobrenome
println(nomeCompleto)
```

A saída desse código será "JoãoSilva". No entanto, se desejamos adicionar um espaço entre o primeiro e sobrenome, podemos utilizar o método "plus()" ao invés do operador "+":

```Kotlin
val nomeCompleto = nome.plus(" ").plus(sobrenome)
```

Dessa forma, a saída será "João Silva". Além disso, podemos utilizar o operador "+=" para adicionar um valor à própria variável em vez de criar uma nova. Veja o exemplo:

```Kotlin
var nome = "Maria"
nome += " Souza"
println(nome)
```

A saída será "Maria Souza". Outra forma interessante de concatenar strings é utilizando interpolação, que permite adicionar valores de variáveis diretamente em uma string. Veja o exemplo:

```Kotlin
val idade = 25
println("Eu tenho $idade anos.")
```

A saída será "Eu tenho 25 anos." Além disso, podemos utilizar o caractere especial "$" para adicionar expressões ou cálculos dentro da string, como no exemplo abaixo:

```Kotlin
val preco = 5
println("O valor total é ${preco * 3} reais.")
```

A saída será "O valor total é 15 reais."

## Aprofundando

Existem algumas coisas importantes a serem consideradas ao concatenar strings em Kotlin. Primeiramente, é importante saber que strings são imutáveis, ou seja, não podem ser modificadas diretamente. Quando utilizamos o operador "+", na verdade estamos criando uma nova string. Por isso, é importante utilizar o método "plus()" ou o operador "+=" quando desejamos adicionar valores à própria variável.

Além disso, é importante ter em mente que a concatenação de strings em loops ou em grandes quantidades pode impactar a performance do código, já que está sendo criada uma nova string a cada iteração. Nesse caso, é recomendado utilizar a classe StringBuilder, que permite modificar a string diretamente sem a necessidade de criar uma nova a cada vez.

## Veja também

- Documentação oficial sobre strings em Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Artigo sobre concatenação de strings em Kotlin: https://blog.kotlin-academy.com/string-concatenation-in-kotlin-1d870093ed5a
- Vídeo tutorial sobre manipulação de strings em Kotlin: https://www.youtube.com/watch?v=_nMcfNWSeTM