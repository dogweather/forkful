---
title:                "Go: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que?

Muitas vezes, ao escrever código em Go, podemos nos deparar com problemas e bugs difíceis de identificar. É aí que a impressão de saída de depuração pode ser extremamente útil. Isso nos permite visualizar o fluxo do nosso código e entender exatamente o que está acontecendo, tornando a depuração muito mais fácil.

## Como Fazer

Para imprimir saída de depuração em Go, podemos usar a função `fmt.Println()` com qualquer valor que desejarmos imprimir. Por exemplo:

```Go
fmt.Println("Olá, mundo!")
```

Isso imprimirá a mensagem "Olá, mundo!" no console. Além disso, podemos usar a função `fmt.Printf()` para formatar a saída de uma maneira mais específica. Por exemplo:

```Go
num1 := 10
num2 := 20
fmt.Printf("A soma de %d e %d é igual a %d", num1, num2, num1+num2)
```

Este código imprimirá a mensagem "A soma de 10 e 20 é igual a 30". Isso pode ser útil quando queremos imprimir valores de variáveis ou expressões específicas para entender melhor o comportamento do nosso código.

## Deep Dive

Existem algumas coisas importantes a serem lembradas ao imprimir saída de depuração em Go. Primeiramente, é importante lembrar de remover ou comentar todas as saídas de depuração antes de enviar o código para produção. Isso garantirá que nosso código não seja desnecessariamente lotado com mensagens de depuração.

Também é importante notar que podemos usar a função `panic()` ao imprimir saída de depuração em situações mais críticas. Isso não apenas imprimirá nossa mensagem, mas também encerrará a execução do programa.

## Veja Também

- [Documentação oficial do Go sobre saída de depuração] (https://golang.org/doc/effective_go.html#logging)
- [Vídeo tutorial sobre impressão de saída de depuração em Go] (https://www.youtube.com/watch?v=rqsLFpTTNjo)
- [Artigo sobre melhores práticas de impressão de saída de depuração em Go] (https://www.ardanlabs.com/blog/2014/10/level-logger-based-in-go.html)