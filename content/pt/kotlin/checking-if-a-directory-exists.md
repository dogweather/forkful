---
title:                "Verificando se um diretório existe"
html_title:           "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Sempre que precisamos trabalhar com arquivos em um programa, é importante verificar se o diretório necessário existe ou não. Isso evita possíveis erros e garante que o código funcione corretamente.

## Como fazer

Para verificar se um diretório existe em Kotlin, podemos utilizar a função `exists()` da classe `File`. Veja um exemplo:

```Kotlin
val diretorio = File("caminho/do/diretorio")
if (diretorio.exists()) {
    println("O diretório existe!")
} else {
    println("O diretório não existe.")
}
```

Se o diretório existir, o código imprimirá "O diretório existe!". Caso contrário, será impresso "O diretório não existe."

## Mergulho Profundo

A função `exists()` retorna um valor booleano indicando se o diretório existe ou não. É importante lembrar que essa função verifica apenas o diretório, não o conteúdo dentro dele. Além disso, ela também pode retornar `false` se o diretório não for acessível devido a permissões de leitura ou escrita.

Outra forma de verificar se um diretório existe é utilizando a função `isDirectory()` da classe `File`. Essa função retorna `true` se o objeto `File` se refere a um diretório existente.

## Veja também

- [Documentação oficial da classe `File`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Como verificar se um arquivo existe em Kotlin](https://www.baeldung.com/kotlin/check-file-exists)