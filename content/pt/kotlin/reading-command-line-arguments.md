---
title:                "Kotlin: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando é importante

Ler argumentos de linha de comando é uma habilidade essencial para qualquer programador Kotlin. Isso permite que você crie aplicativos mais dinâmicos e interativos, permitindo ao usuário personalizar a execução do seu código.

## Como ler argumentos de linha de comando em Kotlin

Ler argumentos de linha de comando em Kotlin é muito simples. Primeiro, vamos ver como obter os argumentos passados ao executar o programa:

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
       println("Os argumentos passados foram: ")
       for (arg in args) {
           println(arg)
       }
    } else {
       println("Nenhum argumento foi passado.")
    }
}
```
```
Exemplo de saída:
> kotlin Programa.kt argumento1 argumento2
Os argumentos passados foram:
argumento1
argumento2
```

Você também pode obter argumentos específicos, por meio do índice do array `args`:

```Kotlin
fun main(args: Array<String>) {
    val primeiroArgumento = args[0]
    val segundoArgumento = args[1]
    // Restante do seu código aqui
}
```

**Importante:** Certifique-se de tratar possíveis exceções ao obter argumentos específicos, como índices inválidos ou tipos de dados incompatíveis.

## Explorando ainda mais a leitura de argumentos de linha de comando

Além de obter e tratar os argumentos, também é possível realizar a validação deles. Por exemplo, você pode garantir que o usuário passe um número como argumento ao invés de uma string ou outro tipo de dado. Isso pode ser feito por meio do `when` expression:

```Kotlin
fun main(args: Array<String>) {
    when {
        args[0].isNumeric() -> executarAlgo()
        else -> println("O primeiro argumento deve ser numérico.")
    }
}
```

Outro aspecto importante é a possibilidade de utilizar a biblioteca `ArgParser`, que facilita a obtenção e validação dos argumentos de linha de comando. Dessa forma, você pode se concentrar na lógica do seu programa, deixando a parte de leitura e validação dos argumentos por conta da biblioteca.

## Veja também
- [Documentação oficial do Kotlin: Reading Program Arguments](https://kotlinlang.org/docs/reference/using-command-line.html#reading-program-arguments)
- [Kotlin Programming Cookbook: Dealing with Command-Line Arguments](https://www.amazon.com/dp/B0892QXVT4)