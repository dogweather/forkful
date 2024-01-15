---
title:                "Imprimindo saída de depuração"
html_title:           "Go: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já teve dificuldades para encontrar e corrigir bugs em seu código? Às vezes, simplesmente inspecionar variáveis ou trechos de código pode não ser suficiente. É aí que entra a impressão de saída de depuração (debug output). Imprimir mensagens de depuração pode ajudar os programadores a entenderem melhor o fluxo do código e identificarem erros com mais facilidade.

## Como Fazer

Em Go, podemos imprimir mensagens de depuração usando a função `fmt.Println()`. Essa função aceita um ou mais argumentos e os imprime no console. Vamos ver um exemplo:

```
package main

import "fmt"

func main() {
    nome := "João"
    fmt.Println("Olá,", nome + "! Seja bem-vindo.")
}
```

Neste exemplo, usamos a função `fmt.Println()` para imprimir uma mensagem de boas-vindas com o nome fornecido pelo usuário. Ao executar este código, veremos a seguinte saída no console:

```
Olá, João! Seja bem-vindo.
```

Além do `fmt.Println()`, também temos a opção de usar a função `fmt.Printf()`, que permite formatar a saída de acordo com especificadores de formato, como `%s` para strings e `%d` para inteiros. Vejamos outro exemplo:

```
package main

import "fmt"

func main() {
    numero1 := 10
    numero2 := 5
    fmt.Printf("%d é maior que %d.", numero1, numero2)
}
```

Nesta função, usamos `%d` para especificar que queremos imprimir os números na saída. Novamente, ao executar este código, veremos a seguinte saída:

```
10 é maior que 5.
```

## Mergulho Profundo

A impressão de saída de depuração (debug output) pode ser especialmente útil em situações em que queremos verificar o valor de variáveis durante a execução do programa. No entanto, é importante lembrar de remover essas mensagens de depuração ao finalizar o desenvolvimento e antes de enviar o código para produção.

Também é possível usar a função `log` da biblioteca padrão do Go para imprimir mensagens de depuração em níveis diferentes, permitindo que o programador controle a gravidade das mensagens. Para saber mais sobre essa funcionalidade, você pode consultar a documentação oficial do Go.

## Veja Também

- [Documentação oficial do Go (em português)](https://golang.org/doc/)
- [Tutorial do Go (em português)](https://go-tour-br.appspot.com/)
- [The Go Blog (em inglês)](https://blog.golang.org/)