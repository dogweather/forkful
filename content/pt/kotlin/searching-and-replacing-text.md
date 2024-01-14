---
title:    "Kotlin: Buscando e substituindo texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que procurar e substituir texto é importante para programadores

Procurar e substituir texto é uma tarefa comum em programação que pode economizar muito tempo e esforço. É útil para fazer alterações em blocos de código grandes e repetitivos, bem como para corrigir erros ortográficos ou atualizar dados em massa. Neste artigo, vamos explorar como realizar essas tarefas em Kotlin.

## Como fazer em Kotlin

A primeira etapa para procurar e substituir texto em Kotlin é importar a classe `Regex` no seu projeto. Em seguida, você pode usar o método `replace` para substituir uma expressão regular por uma nova cadeia de caracteres. Por exemplo:

```Kotlin
val texto = "Este é um exemplo de substituição de texto em Kotlin."
val novoTexto = texto.replace(Regex("Kotlin"), "Java")
println(novoTexto) // saída: Este é um exemplo de substituição de texto em Java.
```

Você também pode usar o método `replaceFirst` para substituir apenas a primeira ocorrência da string. E se você quiser salvar a nova string em uma variável, basta usar o operador de atribuição `=`.

```Kotlin
val texto = "Este é um exemplo de substituição de texto em Kotlin."
val novaString = texto.replaceFirst(Regex("em Kotlin"), "com Java")
println(novaString) // saída: Este é um exemplo de substituição de texto com Java.
```

Você pode usar expressões regulares mais complexas para procurar e substituir padrões específicos em uma string. Por exemplo, se você quiser substituir todos os números por asteriscos, você pode fazer o seguinte:

```Kotlin
val texto = "12345 abcdef"
val novoTexto = texto.replace(Regex("\\d"), "*")
println(novoTexto) // saída: ***** abcdef
```

Note que precisamos adicionar uma barra invertida antes do dígito para indicar que é um caractere especial na expressão regular. Esta é apenas uma das muitas maneiras de usar expressões regulares para procurar e substituir texto em Kotlin.

## Aprofundando-se em busca e substituição de texto

Embora as expressões regulares sejam uma maneira poderosa e flexível de procurar e substituir texto, elas também podem ser difíceis de entender no começo. Felizmente, existem muitos recursos on-line úteis para aprender mais sobre expressões regulares em Kotlin. Aqui estão alguns links para você explorar:

- [Documentação oficial do Kotlin sobre expressões regulares](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [Tutorial do Vogella sobre expressões regulares em Kotlin](https://www.vogella.com/tutorials/KotlinRegularExpressions/article.html)
- [Exemplos interativos de expressões regulares em Kotlin](https://regex101.com/library/am7s8V)

# Ver também

- [Tutorial do Kotlin: Como fazer string interpolation](https://kotlinlang.org/docs/strings.html#string-interpolation)
- [Site oficial do Kotlin](https://kotlinlang.org/)