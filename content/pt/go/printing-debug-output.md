---
title:                "Go: Imprimindo saída de depuração"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saídas de depuração em Go?

Imprimir saídas de depuração é uma prática comum na programação em Go. Isso é feito para verificar o estado das variáveis e acompanhar o fluxo de execução de um programa. Isso é especialmente útil ao desenvolver e depurar código, pois pode ajudar a identificar erros e entender o comportamento do programa.

## Como fazer

Para imprimir saídas de depuração em Go, você pode usar a função `fmt.Println()` ou `fmt.Printf()`, que são parte da biblioteca padrão do Go. Você pode passar variáveis como argumentos para essas funções para imprimir seus valores. Por exemplo:

```Go
// Exemplo de saída de depuração com fmt.Println()
var number = 10
fmt.Println("O valor do número é:", number)

// Exemplo de saída de depuração com fmt.Printf()
var name = "Maria"
fmt.Printf("Olá %s, seja bem-vindo!", name)
```

A saída para esse código seria:

```
O valor do número é: 10
Olá Maria, seja bem-vindo!
```

Você também pode usar a função `fmt.Sprintf()` para formatar uma string e armazená-la em uma variável para uso posterior. Isso é útil quando você precisa imprimir um valor mais de uma vez no seu código. Por exemplo:

```Go
// Exemplo de uso de fmt.Sprintf()
var number = 5
var message = fmt.Sprintf("O dobro de %d é %d", number, number*2)
fmt.Println(message)
```
A saída para esse código seria: `O dobro de 5 é 10`.

## Aprofundando-se

Há outras opções para imprimir saídas de depuração em Go, como a função `log.Print()`, que permite adicionar um nível de registro para a saída e pode ser útil ao rastrear erros. Além disso, você também pode usar a diretiva `DEBUG` para imprimir saídas de depuração somente quando um programa é executado em modo de depuração.

Uma dica importante é usar o pacote `log` em vez do pacote `fmt`, pois ele fornece mais recursos e pode tornar a depuração de problemas mais eficiente.

## Veja também

- [Documentação oficial sobre a função fmt](https://golang.org/pkg/fmt/)
- [Tutorial sobre depuração em Go](https://blog.golang.org/debuggers)
- [Tutorial sobre o pacote log](https://www.geeksforgeeks.org/golang-log-package/)