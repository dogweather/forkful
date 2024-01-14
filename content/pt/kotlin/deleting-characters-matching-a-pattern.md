---
title:    "Kotlin: Excluir caracteres que correspondem a um padrão"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando com strings em nossos programas, muitas vezes precisamos limpar e formatar esses dados para que possam ser usados da maneira correta. Uma das tarefas mais comuns é excluir caracteres ou padrões específicos de uma string. Há várias razões pelas quais alguém pode querer fazer isso, como remover espaços em branco desnecessários, caracteres especiais ou até mesmo números.

## Como fazer

Usando Kotlin, existem algumas maneiras de excluir caracteres correspondentes a um padrão em uma string. Aqui estão alguns exemplos de código que mostram diferentes abordagens:

```Kotlin
// Usando regex para excluir espaços em branco
val string = "Esta é uma string com espaços em branco"
val novaString = string.replace(Regex("\\s+"), "")
println(novaString)
-> "Estaéumastringcomespaçosembranco"

// Usando replace() com um char
val string = "Olá, mundo!"
val novaString = string.replace("o", "")
println(novaString)
-> "Olá, mund!"

// Usando replaceAll() com uma expressão lambda
val string = "abcdefg"
val novaString = string.replaceAll { it.groupValues[1] }
println(novaString)
-> "aceg"
```
Note que cada um desses métodos tem suas próprias vantagens e desvantagens, e a escolha depende do contexto e do que você está tentando alcançar. Certifique-se de ler a documentação oficial do Kotlin e experimentar diferentes abordagens.

## Aprofundando

Agora, vamos dar uma olhada mais profunda em como excluir caracteres correspondentes a um padrão em Kotlin. A função `replace()` é útil quando queremos substituir um caractere específico, mas e se quisermos excluir todos os caracteres correspondentes? É aí que entra a classe `Regex`. Podemos definir um padrão específico usando expressões regulares e, em seguida, usá-lo juntamente com a função `replaceAll()` para substituir todos os caracteres correspondentes por uma string vazia. Assim, teremos uma nova string sem os caracteres que especificamos.

Outra coisa importante a ter em mente é que as strings em Kotlin são imutáveis, ou seja, não podemos modificá-las diretamente. Portanto, todas as funções que utilizamos, como `replace()` e `replaceAll()`, retornam uma nova string com as alterações desejadas. Certifique-se de armazenar essa nova string em uma variável para poder usá-la posteriormente.

## Veja também

- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Tutorial de expressões regulares no Kotlin](https://www.baeldung.com/java-kotlin-string-regular-expression)
- [Outras funções úteis para manipulação de strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)