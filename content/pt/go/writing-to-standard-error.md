---
title:    "Go: Escrevendo para o erro padrão"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o fluxo de erro padrão?

Escrever para o fluxo de erro padrão é uma prática comum na programação Go. Isso permite que os desenvolvedores exibam mensagens de erro ou avisos durante a execução de um programa, facilitando a identificação e solução de problemas. Além disso, escrever para o fluxo de erro padrão é uma forma de padronizar as mensagens de erro e tornar o código mais legível e organizado.

## Como fazer

Ao escrever para o fluxo de erro padrão em Go, é necessário importar o pacote `os`. Em seguida, é possível utilizar a função `os.Stderr.WriteString()` para enviar uma string para o fluxo de erro padrão. Por exemplo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Escreve uma mensagem de erro no fluxo de erro padrão
    os.Stderr.WriteString("Ocorreu um erro ao executar o programa!")
    
    fmt.Println("Continuando a execução do programa...")
}
```

A saída desse programa seria:

```
Ocorreu um erro ao executar o programa!
Continuando a execução do programa...
```

Além disso, você também pode usar a função `fmt.Fprintf()` para formatar suas mensagens de erro antes de enviá-las para o fluxo de erro padrão. Por exemplo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    num1 := 10
    num2 := 0
    
    // Verifica se o segundo número é igual a 0
    if num2 == 0 {
        // Escreve uma mensagem formatada de erro no fluxo de erro padrão
        fmt.Fprintf(os.Stderr, "Erro: o segundo número (%d) é igual a 0!", num2)
        os.Exit(1) // Encerra o programa com status de erro
    }
}
```

A saída desse programa seria:

```
Erro: o segundo número (0) é igual a 0!
```

## Profundidade

Ao escrever para o fluxo de erro padrão, é importante considerar alguns aspectos importantes. Primeiramente, mensagens de erro devem ser claras e descritivas o suficiente para ajudar outros desenvolvedores a entender o problema e encontrar uma solução. Por isso, é importante incluir informações relevantes sobre o erro, como qual função ou linha de código causou o problema.

Além disso, é recomendável utilizar o pacote `errors` para criar erros personalizados, com mensagens mais detalhadas e específicas sobre o problema. Dessa forma, é possível personalizar as mensagens de acordo com a necessidade do programa.

Outra dica importante é evitar o uso excessivo de mensagens de erro. Isso pode sobrecarregar o fluxo de erro padrão e dificultar a identificação dos erros importantes. O ideal é utilizar mensagens de erro apenas em situações críticas ou em casos que exijam uma explicação mais detalhada.

## Veja também

- [Pacote `os` da documentação oficial do Go](https://golang.org/pkg/os/)
- [Pacote `fmt` da documentação oficial do Go](https://golang.org/pkg/fmt/)
- [Pacote `errors` da documentação oficial do Go](https://golang.org/pkg/errors/)
- [Tutorial sobre fluxo de erro padrão em Go](https://blog.golang.org/error-handling-and-go) (em inglês)