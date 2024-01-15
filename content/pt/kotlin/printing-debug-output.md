---
title:                "Imprimindo saída de depuração"
html_title:           "Kotlin: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que 

É comum que durante o processo de desenvolvimento de um programa, precisemos verificar o estado das variáveis e como elas estão interagindo. A impressão de saída de depuração, ou debug output, é uma maneira eficaz e simples de visualizar essas informações e facilitar o processo de solução de problemas. 

## Como Fazer

Para imprimir saída de depuração no Kotlin, usamos a função ```println()``` ou ```print()``` para exibir informações em tempo de execução. Por exemplo: 

```Kotlin 
var nome = "João"
var sobrenome = "Silva"
println("Olá, meu nome é $nome $sobrenome")
```

A saída esperada será "Olá, meu nome é João Silva". Também podemos usar a função ```debug()``` da classe ```kotlin.Debug``` para imprimir informações de depuração em um formato mais detalhado. 

## Mergulho Profundo

Ao usar a função ```debug()```, podemos especificar o nível de detalhe que queremos imprimir, como variáveis, tempo de execução, pilha de chamadas, entre outros. Podemos até mesmo criar nossa própria estratégia de impressão personalizada com a função ```printDebugAsString()```. Além disso, é importante lembrar de remover qualquer saída de depuração antes de lançar o programa em produção. 

## Veja Também 

- Documentação Oficial do Kotlin: https://kotlinlang.org/docs/tutorials/command-line.html#create-and-run-a-project 
- Tutorial prático de Kotlin: https://kotlinlang.org/docs/tutorials/ 
- Dicas de depuração em Kotlin: https://blog.jetbrains.com/kotlin/2018/06/kotlin-debugging-tips-part-1/