---
title:    "Kotlin: Extraindo subtrings"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que utilizar extração de substrings em Kotlin?

Extrair substrings é uma técnica muito útil e comum em programação para obter partes específicas de uma string. Ao dominar essa técnica, você poderá manipular suas strings de forma mais eficiente e criar soluções mais complexas em suas aplicações. Além disso, é uma habilidade fundamental para quem deseja aprender a linguagem Kotlin.

## Como fazer isso em Kotlin

Para extrair substrings em Kotlin, basta utilizar a função `subSequence()` em uma string, passando os índices inicial e final da substring desejada. Veja um exemplo abaixo:

```Kotlin
val frase = "Extraindo substrings é tão fácil quanto isso"
val substring = frase.subSequence(10, 19)
println(substring) // "substrings"
```

Você também pode utilizar a função `substring()` para obter a mesma saída:

```Kotlin
val frase = "Extraindo substrings é tão fácil quanto isso"
val substring = frase.substring(10, 19)
println(substring) // "substrings"
```

## Mergulho profundo na extração de substrings

Ao utilizar a função `substring()` em Kotlin, é importante ter em mente que o índice final não é incluído na substring resultante. Ou seja, no exemplo acima, a substring "substring" tem os índices 10 e 18, excluindo o último índice que seria 19.

Além disso, é possível passar apenas um índice para a função `substring()`, indicando o início da substring até o final da string original. Por exemplo:

```Kotlin
val frase = "Extraindo substrings é tão fácil quanto isso"
val substring = frase.substring(10)
println(substring) // "substrings é tão fácil quanto isso"
```

Outra opção é utilizar o operador `..` para indicar o intervalo de índices desejado, como mostrado no exemplo abaixo:

```Kotlin
val frase = "Extraindo substrings é tão fácil quanto isso"
val substring = frase[10..18] // utiliza o caractere [10] até [18]
println(substring) // "substrings"
```

Essa técnica também pode ser utilizada para obter substrings de dentro de um loop, por exemplo:

```Kotlin
val nome = "Pedro"
for (i in 1 until nome.length) {
    val substring = nome[i..nome.length - 1]
    println(substring) // "edro", "dro", "ro", "o"
}
```

# Veja também

- Documentação oficial de strings em Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Tutorial de Kotlin sobre manipulação de strings: https://codelabs.developers.google.com/codelabs/kotlin-android-training-text/#1
- Video tutorial sobre extração de substrings em Kotlin: https://www.youtube.com/watch?v=l5mupVdlTjI