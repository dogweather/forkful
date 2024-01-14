---
title:    "Go: Imprimindo saída de depuração"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que imprimir mensagens de depuração em seu código Go

Muitas vezes, durante o desenvolvimento de um programa, nos deparamos com erros ou bugs que podem ser difíceis de identificar e corrigir. É aí que entra a impressão de mensagens de depuração. Ao imprimir informações específicas sobre o estado do programa em determinados pontos do código, podemos entender melhor o que está acontecendo e assim, encontrar e corrigir os problemas de forma mais eficiente.

## Como imprimir mensagens de depuração em Go

Para imprimir mensagens de depuração em Go, podemos utilizar a função `fmt.Printf()`, que funciona de maneira semelhante ao `printf()` em linguagens como C e Java.

```Go
package main

import "fmt"

func main() {
  nome := "Maria"
  fmt.Printf("Olá, %s! Seu nome possui %d letras.", nome, len(nome))
}
```

Neste exemplo, usamos o verbo `%s` para especificar que o valor da variável `nome` deve ser substituído na mensagem, e `%d` para o tamanho do nome de Maria. A saída seria:

```
Olá, Maria! Seu nome possui 5 letras.
```

Também é possível utilizar `fmt.Print()` para imprimir mensagens sem formatação ou `fmt.Println()` para adicionar uma quebra de linha no final.

## Profundando na impressão de mensagens de depuração

Além de simplesmente imprimir valores de variáveis, também podemos utilizar mensagens de depuração em Go para verificar se determinados trechos do código estão sendo executados corretamente. Um exemplo seria imprimir uma mensagem logo antes e após uma condição ser testada ou um laço de repetição ser executado.

Outra forma útil de utilizar mensagens de depuração é imprimir os valores de retorno de funções em diferentes pontos do programa para entender melhor o fluxo de execução.

Também podemos usar a função `fmt.Sprintf()` para criar strings formatadas e armazená-las em uma variável, em vez de imprimi-las diretamente na tela. Isso pode ser útil ao imprimir várias informações juntas em uma única mensagem de depuração.

## Veja também

- [Documentação oficial do pacote fmt em Go](https://golang.org/pkg/fmt/)
- [Ótimos recursos para ajudar na depuração de código em Go](https://medium.com/swlh/debugging-go-code-5-tips-and-tricks-b309c9f44d26)
- [Vídeo tutorial sobre impressão de mensagens de depuração em Go](https://www.youtube.com/watch?v=CF9S4QZuV30)