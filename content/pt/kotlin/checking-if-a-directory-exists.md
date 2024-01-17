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

## O que é e Por que Fazer?
Verificar se um diretório existe é simplesmente garantir que um caminho fornecido de arquivo ou diretório é válido e corresponde a uma pasta existente no sistema. Os programadores fazem isso para evitar erros e garantir que seus programas possam acessar e manipular os arquivos e diretórios necessários para o seu funcionamento.

## Como Fazer:
```
Kotlin
fun main() {
    val path = "C:/Users/username/Documents"
    val directory = File(path)
    if(directory.exists()){
        println("O diretório existe!")
    } else {
        println("O diretório não existe!")
    }
}
```
Output: O diretório existe!

## Mergulho Profundo:
Verificar a existência de um diretório pode parecer uma tarefa simples, mas é uma etapa importante no desenvolvimento de qualquer aplicativo ou programa que lida com arquivos e diretórios. Antes do lançamento da linguagem Kotlin, os programadores tinham que recorrer a bibliotecas externas ou utilizar um método mais complexo envolvendo a classe Path da linguagem Java para realizar essa tarefa. Felizmente, com Kotlin, temos um método simples e fácil de usar para verificar a existência de um diretório. Além disso, também é importante ressaltar que verificar a existência de um diretório é diferente de verificar se um arquivo existe, pois os métodos e as classes utilizadas são diferentes. 

## Veja Também:
- [Documentação do Kotlin sobre a classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Documentação do Java sobre a classe Path](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Path.html)