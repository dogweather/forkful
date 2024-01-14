---
title:                "Kotlin: Lendo argumentos da linha de comando."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade importante para qualquer programador, pois permite que o usuário interaja com o programa de forma mais dinâmica e personalizada. Além disso, isso pode facilitar a execução de diferentes ações sem a necessidade de acessar diretamente o código fonte do programa.

## Como fazer?

Para ler argumentos da linha de comando em Kotlin, você precisará usar o objeto `args` para acessar a lista de argumentos passados. Por exemplo, se você quiser imprimir o primeiro argumento passado ao programa, pode fazer o seguinte:

```Kotlin
fun main(args: Array<String>) {
    println("O primeiro argumento é: ${args[0]}")
}
```

Se você executar o programa com o argumento "Olá", o output será:

```
O primeiro argumento é: Olá
```

Para lidar com múltiplos argumentos, pode-se percorrer a lista usando um loop `for`:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println("Argumento: $arg")
    }
}
```

Se você executar o programa com dois argumentos, "Olá" e "mundo", o output será:

```
Argumento: Olá
Argumento: mundo
```

## Mergulho profundo

Além dos códigos acima, há também formas para verificar se um argumento específico foi passado ou se o número de argumentos esperados foi respeitado. Por exemplo, podemos verificar o número de argumentos usando a propriedade `size` da lista:

```Kotlin
fun main(args: Array<String>) {
    if (args.size != 2) {
        println("Por favor, forneça o primeiro nome e o sobrenome.")
        return
    }
    val nome = args[0]
    val sobrenome = args[1]
    println("Olá, $nome $sobrenome")
}
```

Agora, se você executar o programa sem passar os dois argumentos esperados, será exibida a mensagem de aviso.

## Veja também

- [Documentação oficial do Kotlin sobre argumentos da linha de comando](https://kotlinlang.org/docs/command-line.html)
- [Tutorial detalhado sobre melhorias de argumentos da linha de comando no Kotlin 1.4](https://kotlinexpertise.com/kotlin-1-4-features-command-line-improvements/)
- [Exemplo de aplicação Kotlin com argumentos da linha de comando](https://github.com/carloscds/songs-list-kotlin)