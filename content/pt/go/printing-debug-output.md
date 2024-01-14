---
title:    "Go: Impressão de saída de depuração"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração em Go

Debugging, ou depuração, é uma parte essencial do processo de desenvolvimento de software. Ao imprimir saída de depuração, podemos visualizar o fluxo do nosso código e encontrar erros e problemas mais facilmente. Isso nos ajuda a entender melhor o funcionamento do nosso programa e a resolver problemas de forma mais eficiente.

## Como imprimir saída de depuração em Go

Para imprimir saída de depuração em Go, podemos usar a função `fmt.Printf()` ou `fmt.Println()`. Vamos supor que temos uma variável `nome` que queremos imprimir para fins de depuração. Podemos fazer isso da seguinte maneira:

```Go
nome := "João"
fmt.Printf("O valor da variável nome é: %s\n", nome)
```

O `%s` é um especificador de formato que indica que queremos imprimir uma string. Podemos usar outros especificadores de formato, dependendo do tipo de dado que queremos imprimir. Por exemplo, `%d` para inteiros e `%f` para números de ponto flutuante.

Além disso, podemos usar a função `fmt.Sprintf()` para atribuir a saída de depuração a uma variável, ao invés de imprimir diretamente na tela:

```Go
debug := fmt.Sprintf("O valor da variável nome é: %s", nome)
//fazer algo com a string debug
```

## Deep Dive: Mais informações sobre a impressão de saída de depuração

Além das funções `fmt.Printf()` e `fmt.Println()`, Go também possui outras ferramentas úteis para imprimir saída de depuração. Por exemplo, podemos usar a biblioteca `log` para imprimir mensagens de log em diferentes níveis de severidade. Isso pode ser útil para depurar problemas em diferentes partes do código.

Outra ferramenta útil é o pacote `spew`, que fornece funções para imprimir estruturas de dados complexas de forma bastante legível e organizada. Isso pode ser especialmente útil quando estamos lidando com dados grandes e complexos.

## Veja também

- [A documentação oficial sobre as funções de impressão de formato em Go](https://golang.org/pkg/fmt/)
- [Um tutorial sobre depuração em Go usando a função `fmt.Printf()`](https://www.calhoun.io/how-to-debug-go-code-with-printf/)
- [O pacote `log` em Go](https://golang.org/pkg/log/)
- [O pacote `spew` em Go](https://github.com/davecgh/go-spew)