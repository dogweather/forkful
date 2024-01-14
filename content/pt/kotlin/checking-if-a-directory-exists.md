---
title:    "Kotlin: Verificando se um diretório existe"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é importante para garantir que o seu código funcione corretamente e evite erros desnecessários. Além disso, pode ser útil para verificar se determinado diretório está disponível antes de tentar acessá-lo ou realizar operações nele.

## Como verificar se um diretório existe

Para verificar se um diretório existe em Kotlin, podemos utilizar a classe `File` da biblioteca padrão do Kotlin. Devemos passar o caminho do diretório a ser verificado como parâmetro para o construtor da classe `File` e em seguida chamar o método `exists()` para verificar se o diretório de fato existe.

```Kotlin
val directory = File("caminho/do/diretorio")

if(directory.exists()){
    println("O diretório existe!")
} else {
    println("O diretório não existe.")
}
```

Se o diretório existir, o código imprimirá "O diretório existe!". Se o diretório não existir, será impresso "O diretório não existe.".

## Mais informações sobre a verificação de diretórios

Além do método `exists()`, a classe `File` possui outros métodos úteis para verificar diretórios, como por exemplo `isDirectory()` para checar se o objeto é um diretório e `canRead()` e `canWrite()` para verificar se é possível ler e escrever no diretório, respectivamente.

É importante também lembrar que a classe `File` pode lançar uma exceção `SecurityException` caso o acesso ao diretório seja restrito por questões de segurança.

## Veja também

- [Documentação oficial do Kotlin: Verificando se um diretório existe](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
- [Tutorial sobre como checar se um diretório existe em Kotlin](https://blog.kotlin-academy.com/kotlin-programming-fundamentals-bd8700125491?gi=2d7eb97388c7)