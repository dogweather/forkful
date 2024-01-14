---
title:                "Kotlin: Verificando se um diretório existe"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe no Kotlin?

Existem muitas razões pelas quais um desenvolvedor Kotlin pode precisar verificar se um diretório existe. Pode ser necessário garantir que o diretório de destino esteja vazio antes de mover ou copiar arquivos, ou pode ser necessário verificar antes de criar um novo diretório para evitar erros. Independentemente da razão, saber como verificar a existência de um diretório no Kotlin é uma habilidade útil para qualquer programador.

## Como fazer a verificação de existência de um diretório no Kotlin

Para verificar se um diretório existe no Kotlin, podemos utilizar a função `exists()` do objeto `File`. Esta função retorna um valor booleano, true se o diretório existir e false se não existir. Veja o código de exemplo abaixo:

```Kotlin
val diretorio = File("/caminho/do/diretorio")

if(diretorio.exists()){
    println("O diretório já existe")
} else {
    println("O diretório não existe")
}
```

O código acima irá imprimir "O diretório já existe" se o diretório especificado existir no sistema de arquivos e "O diretório não existe" se não existir. É importante notar que a função `exists()` também irá retornar false se o diretório especificado for um arquivo ao invés de um diretório.

## Uma visão mais profunda da verificação de existência de um diretório

Por trás das cortinas, a função `exists()` utiliza outras funções do objeto `File` para realizar a verificação. Primeiro, ela utiliza a função `getAbsoluteFile()` para obter uma referência absoluta do diretório especificado, a fim de evitar quaisquer erros devido a caminhos relativos ou simbólicos. Em seguida, ela utiliza a função `canRead()` para verificar se o diretório pode ser lido pelo programa. Se ambas as verificações forem bem sucedidas, então a função `exists()` irá retornar true.

É importante notar que essa verificação de existência é apenas no momento em que a função é chamada. Se o diretório for criado ou excluído após a verificação, o resultado da função `exists()` pode ser diferente. Por isso, é uma boa prática sempre realizar essa verificação antes de realizar qualquer operação em um diretório.

## Veja também

- [Documentação oficial do Kotlin sobre a classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Exemplo de código completo no GitHub](https://github.com/exemplo-link-para-codigo-de-verificacao-de-existencia-de-diretorio)