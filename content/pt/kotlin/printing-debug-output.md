---
title:    "Kotlin: Imprimindo saída de depuração"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Os desenvolvedores muitas vezes precisam lidar com código complexo que pode resultar em erros difíceis de identificar. Ao imprimir saída de depuração, é possível visualizar as variáveis e dados em um determinado ponto do programa, facilitando a compreensão do fluxo de execução e a identificação de possíveis problemas.

## Como fazer

Para imprimir uma saída de depuração em Kotlin, utilizamos a função `println()` e passamos como parâmetro o que desejamos imprimir. Por exemplo, se quisermos imprimir o valor de uma variável `idade`, podemos utilizar o seguinte código:

```Kotlin
println("Idade: $idade")
```

Para imprimir vários valores em uma única linha, basta utilizar o operador `+` para concatená-los, como no exemplo a seguir:

```Kotlin
println("Nome: $nome, Idade: $idade")
```

O resultado da execução desses códigos será a impressão da saída de depuração no terminal ou console.

```
Idade: 27
Nome: João, Idade: 27
```

## Mergulho profundo

Além da função `println()`, também é possível utilizar a função `print()` para imprimir uma saída de depuração sem quebra de linha. Além disso, é possível utilizar a função `debug()` da biblioteca Kotlin Standard Library para imprimir valores durante a execução do programa.

É importante lembrar de remover todas as chamadas de saída de depuração antes de lançar uma versão final do código, para evitar qualquer impacto no desempenho da aplicação.

## Veja também

- Documentação oficial do Kotlin sobre a função [println()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)
- Tutorial sobre [impressão de saída de depuração em Kotlin](https://blog.kotlin-academy.com/kotlin-tips-printing-stuff-debug-4500297b9095)