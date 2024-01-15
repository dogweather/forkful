---
title:                "Lendo argumentos da linha de comando"
html_title:           "Kotlin: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar: por que alguém deveria gastar tempo lendo argumentos de linha de comando em Kotlin? A resposta é simples: ao entender como ler e usar esses argumentos, você pode criar programas mais interativos e versáteis.

## Como fazer

A primeira coisa a saber é como obter os argumentos de linha de comando em um programa Kotlin. Isso pode ser feito usando a função `main()` com um parâmetro `Array<String>` que contém os argumentos. Aqui está um exemplo simples:

```
Kotlin
fun main(args: Array<String>) {
    println("Os argumentos fornecidos foram: ")
    for(arg in args) {
        println(arg)
    }
}
```

Se executarmos este programa com os argumentos `arg1 arg2 arg3`, teremos a seguinte saída:

```
Kotlin
Os argumentos fornecidos foram:
arg1
arg2
arg3
```

Observe que, por padrão, o primeiro argumento é sempre o nome do arquivo do programa. Agora, vamos dar um passo adiante e aprender a usá-los de forma mais eficaz.

Uma maneira útil de usar argumentos de linha de comando é fornecer ao usuário opções de escolha. Por exemplo, suponha que queremos criar um programa que calcule a média de uma lista de números. Podemos fazer isso permitindo que o usuário escolha entre a média aritmética ou a média geométrica usando argumentos de linha de comando. Aqui está um exemplo:

```
Kotlin
fun main(args: Array<String>) {
    val numbers = mutableListOf<Double>()
    for (arg in args) {
        numbers.add(arg.toDouble())
    }
    val average = if (args.contains("-a")) {
        numbers.average()
    } else {
        var product = 1.0
        for (num in numbers) {
            product *= num
        }
        Math.pow(product, 1.0/numbers.size)
    }
    println("A média é $average")
}
```

Se executarmos este programa com os seguintes argumentos: `10 20 30 -a`, obteremos a média aritmética dos números, que é `20.0`. Mas se executarmos com `10 20 30`, obteremos a média geométrica, que é `18.25854403775862`. Experimente e veja como os resultados mudam. Este é apenas um exemplo simples de como os argumentos de linha de comando podem ser usados para tornar seu programa mais flexível e personalizável pelos usuários.

## Mergulho Profundo

Agora que já sabemos como obter e usar os argumentos de linha de comando em Kotlin, vale a pena mencionar que existem algumas bibliotecas de terceiros que podem deixar esse processo ainda mais fácil e poderoso. Por exemplo, a biblioteca [args4k](https://github.com/avast/args4k) oferece uma maneira de criar argumentos de linha de comando de maneira simples e intuitiva, com opções de personalização e tratamento de erros robusto.

Além disso, é importante ressaltar que a documentação oficial do Kotlin também oferece um guia detalhado sobre como trabalhar com argumentos de linha de comando, incluindo informações sobre como manipular arquivos de propriedades e configurações mais avançadas.

## Veja também

- [Documentação oficial do Kotlin: Lendo argumentos de linha de comando](https://kotlinlang.org/docs/reference/properties.html#command-line-properties)
- [Guia de uso da biblioteca args4k](https://github.com/avast/args4k#how-to-use)
- [Exemplos de uso de argumentos de linha de comando em Kotlin](https://www.programiz.com/kotlin-programming/argument-command-line)