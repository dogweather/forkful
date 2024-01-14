---
title:    "Kotlin: Buscando e substituindo texto"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que
A substituição de texto é uma tarefa comum em programação, especialmente quando se trata de manipulação de strings. Ao aprender como fazer isso em Kotlin, você poderá economizar tempo e esforço em suas tarefas de codificação.

## Como fazer
Para substituir texto em uma string em Kotlin, você pode usar o método `.replace()` em uma instância da classe `String`. Este método aceita dois parâmetros: a string que você deseja substituir e a string que será usada como substituta. Por exemplo:

```Kotlin
val string = "Hello World!"
val novaString = string.replace("Hello", "Olá")
println(novaString)
```
O output será: "Olá World!".

Se você precisar substituir mais de uma ocorrência da string, pode adicionar um terceiro parâmetro opcional para especificar o número máximo de substituições a serem feitas:

```Kotlin
val string = "Hello World, Hello Universe!"
val novaString = string.replace("Hello", "Olá", 1)
println(novaString)
```
O output será: "Olá World, Hello Universe!".

Você também pode usar expressões regulares para substituir texto em uma string. Por exemplo, se você quiser substituir todas as letras maiúsculas por minúsculas em uma string, pode usar o método `.replace()` com uma expressão regular e a função `.toLowerCase()`:

```Kotlin
val string = "Olá Mundo!"
val novaString = string.replace(Regex("[A-Z]"), { it.value.toLowerCase() })
println(novaString)
```

O output será: "olá mundo!".

## Deep Dive
O método `.replace()` aceita uma expressão regular ou uma sequência literal como o primeiro parâmetro. Se for uma expressão regular, ele substituirá todas as ocorrências correspondentes na string. No entanto, se for uma sequência literal, ele substituirá apenas a primeira ocorrência. Você também pode usar uma expressão regular para fazer substituições com base em padrões mais complexos, como combinações de caracteres, espaços em branco ou símbolos específicos.

## Veja também
- [Documentação oficial do Kotlin sobre o método `.replace()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Tutorial do Kotlin sobre expressões regulares](https://kotlinlang.org/docs/regex.html)
- [Artigo sobre manipulação de strings em Kotlin](https://www.raywenderlich.com/7073363-kotlin-strings-tutorial-for-android-getting-started)