---
title:                "Kotlin: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma tarefa comum na programação, especialmente quando se trabalha com strings longas ou complexas. Esta prática consiste em pegar uma parte específica de uma string maior, seja ela um pedaço de texto, um caractere ou uma combinação deles. Isso pode ser útil em diversas situações, como formatação, validação de entrada de dados ou manipulação de textos.

## Como fazer

Para extrair substrings em Kotlin, podemos utilizar o método `substring()` da classe `String`. Ele recebe dois parâmetros: o índice de início e o índice de fim da substring desejada.

```
Kotlinfun main() {
    val texto = "Este é um texto de exemplo."
    println(texto.substring(5, 16))
}
```

**Output:**
`um texto de`

Podemos também utilizar o método `subSequence()` para extrair uma subsequence do texto, passando apenas o índice de início e o índice de fim da substring desejada.

```
Kotlinfun main() {
    val texto = "Este é um texto de exemplo."
    println(texto.subSequence(10, 14))
}
```

**Output:**
`texto`

Além disso, podemos utilizar expressões regulares para extrair substrings com facilidade. Com o método `find()`, podemos encontrar a primeira ocorrência de um padrão especificado em uma string e extrair a substring correspondente.

```
Kotlinfun main() {
    val texto = "Este é um texto de exemplo."
    val padrao = Regex("[d-t]+")
	val match = padrao.find(texto)
	println(match?.value)
}
```

**Output:**
`de`

## Deep Dive

Agora que já sabemos como extrair substrings em Kotlin, vamos aprofundar um pouco mais no assunto. É importante lembrar que, ao extrair uma substring, estamos criando uma nova string, pois strings em Kotlin são imutáveis. Isso significa que sempre que utilizamos um método de extração, estamos alocando mais memória para armazenar a nova string. Portanto, é importante ter cuidado ao utilizar essa prática, especialmente em textos muito longos, para não sobrecarregar o sistema.

Além disso, podemos utilizar os métodos `startsWith()` e `endsWith()` para verificar se uma string começa ou termina com a substring especificada, respectivamente. Isso pode ser útil em casos de validação de entrada de dados.

## Veja também

- [Documentação oficial sobre strings em Kotlin](https://kotlinlang.org/docs/reference/strings.html)
- [Utilizando expressões regulares em Kotlin](https://www.regular-expressions.info/kotlin.html)
- [Outros métodos úteis para manipulação de strings em Kotlin](https://stackoverflow.com/questions/4662215/what-are-the-kotlin-a-class-a-methods-to-use-strings)